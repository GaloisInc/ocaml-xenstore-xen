(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt
open Xs_protocol
open Xenstore_server
open Xs_flask
open Xssm

let debug fmt = Logging.debug "server_xen" fmt
let warn  fmt = Logging.warn  "server_xen" fmt
let error fmt = Logging.error "server_xen" fmt

(* Parse the path database from a bigarray. *)
let parse_path_db arr =
  let rec parse_lines words = function
    | [] -> words
    | line :: rest -> parse_lines ((Junk.String.split ' ' line) @ words) rest
  in
  let rec parse_words values = function
    | [] -> values
    | "ctx" :: path :: ty :: rest -> parse_words ((path,Path_db.Value_str ("system_u:object_r:" ^ ty)) :: values) rest
    | "dom" :: path :: rest -> parse_words ((path,Path_db.Value_domid) :: values) rest
    | rest -> raise (Invalid_argument (List.hd rest)) in
  let str = String.trim (Io_page.to_string arr) in
  Path_db.build_db (parse_words [] (parse_lines [] (Junk.String.split '\n' str)))

(* Parse the context database from a bigarray. *)
let parse_context_db arr =
  let str = String.trim (Io_page.to_string arr) in
  let lines = Junk.String.split '\n' str in
  let parse line db =
    match Junk.String.split ' ' line with
    | "user" :: u1 :: u2 :: [] ->
      Context_db.insert_user u1 u2 db
    | "role" :: r1 :: r2 :: [] ->
      Context_db.insert_role r1 r2 db
    | "type" :: t1 :: t2 :: [] ->
      Context_db.insert_type t1 t2 db
    | [] -> db    (* ignore blank lines *)
    | _  -> raise (Invalid_argument line)
  in
  List.fold_right parse lines Context_db.empty

(* Extract path and context databases and security policy from
 * the ramdisk. *)
let read_ramdisk () =
  let ramdisk = OS.Start_info.((mod_array ())) in
  let read_ramdisk_file filename =
    let arr = Cpio.find_file filename ramdisk in
    match arr with
    | Some x ->
      debug "  %s  %d bytes" filename (Io_page.length x);
      x
    | None ->
      raise (Failure ("file " ^ filename ^ " not found in ramdisk"))
  in
  debug "reading ramdisk (%d bytes):" (Io_page.length ramdisk);
  let policy     = read_ramdisk_file "xenstore.24" in
  let path_db    = read_ramdisk_file "path-db.txt" in
  let context_db = read_ramdisk_file "context-db.txt" in
  (path_db, context_db, policy)

let flask_operations = {
  read = Hooks.flask_read;
  write = Hooks.flask_write;
  create = Hooks.flask_create;
  delete = Hooks.flask_delete;
  chmod = Hooks.flask_chmod;
  relabelfrom = Hooks.flask_relabelfrom;
  relabelto = Hooks.flask_relabelto;
  override = Hooks.flask_override;
  bind = Hooks.flask_bind;
  transition = Hooks.flask_transition;
  introduce = Hooks.flask_introduce;
  stat = Hooks.flask_stat;
  release = Hooks.flask_release;
  resume = Hooks.flask_resume;
  chown_from = Hooks.flask_chown_from;
  chown_to = Hooks.flask_chown_to;
  chown_transition = Hooks.flask_chown_transition;
  retain_owner = Hooks.flask_retain_owner;
  make_priv_for = Hooks.flask_make_priv_for;
  set_as_target = Hooks.flask_set_as_target;
  set_target = Hooks.flask_set_target;
  new_node_label = Hooks.new_node_label;
  get_value_type = Hooks.flask_get_value_type;
  check_domid = Hooks.flask_check_domid;
}

module DomainServer = Xs_server.Server(Xs_transport_domain)

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
			(fun x ->
				(* XXX need access to the console module here *)
				(* lwt () = C.log_s console x in *)
				Printf.printf "%s\n%!" x;
				return ()
			) lines in
	logging_thread logger

(* Parsed command line options. *)
type options = {
  event: int option;
  master_domid: int;
  flask_enable : bool;
  flask_enforcing : bool;
}

let default_options = {
  event = None;
  master_domid = 0;
  flask_enable = true;
  flask_enforcing = true;
}

(* Convert a string to an "int option". *)
let safe_int_of_string s =
  try
    Some (int_of_string s)
  with Failure _ ->
    None

(* Accepts "1" and "true" as true, "0" and "false" as false. *)
let safe_bool_of_string s =
  match s with
  | "1" | "true"  -> true
  | "0" | "false" -> false
  | _  ->
    warn "Invalid boolean argument: %s" s;
    false

(* Check for required options, raising "Failure" if options are missing. *)
let check_required_options opts =
  let check_option name x =
    match x with
    | Some _ -> ()
    | None   -> error "Missing required option: %s" name in
  check_option "--event" opts.event

(* Retrieve a "Some" from an "option" or raise "Failure". *)
let option_get x =
  match x with
  | Some y -> y
  | None -> raise (Failure "option_get None")

(* Parse the command line and return an "options" record.
   Raises "Failure" if any required options are missing. *)
let parse_options () =
  let cmd_line = OS.Start_info.((get ()).cmd_line) in
  let words = Junk.String.split ' ' cmd_line in
  let rec loop opts = function
    | "--event" :: e :: rest ->
      loop { opts with event = safe_int_of_string e } rest
    | "--master-domid" :: d :: rest ->
      loop { opts with master_domid = int_of_string d } rest
    | "--flask-enable" :: x :: rest ->
      loop { opts with flask_enable = safe_bool_of_string x } rest
    | "--flask-enforcing" :: x :: rest ->
      loop { opts with flask_enforcing = safe_bool_of_string x } rest
    | s :: rest ->
      warn "Unknown option: %s" s;
      loop opts rest
    | [] -> opts in
  let opts = loop default_options words in
  check_required_options opts;
  opts

let introduce_dom0 opts =
  let port = option_get opts.event in
  let id = opts.master_domid in
  let mfn = (OS.Start_info.((get ()).store_mfn)) in
  let nmfn = Nativeint.of_int mfn in
  let intro = Introduce.({ domid = id; mfn = nmfn; remote_port = port }) in
  Introduce.introduce intro;
  debug "Introduced domain %d with mfn = 0x%x, port = %d" id mfn port

let main console_mod console =
	debug "Mirage xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in
	let opts = parse_options () in
  let (path_db_arr, context_db_arr, policy_arr) = read_ramdisk () in
  let path_db    = parse_path_db path_db_arr in
  let context_db = parse_context_db context_db_arr in

	if opts.flask_enable then begin
    (* TODO: Have xenstore-flask load the policy? *)
    OS.Sepol.load_policy policy_arr;
    Hooks.set_path_db path_db;
    Hooks.set_context_db context_db;
		Xssm.set_security flask_operations;
		Perms.set_dom0_check_enabled false;
		Hooks.flask_setenforce opts.flask_enforcing
	end;

	Perms.set_dom0_id opts.master_domid;
	let (_: unit Lwt.t) = DomainServer.serve_forever () in
	debug "Started server on xen inter-domain transport";

	introduce_dom0 opts;

	debug "getdomaininfo %d" opts.master_domid;
	begin match OS.Domctl.getdomaininfo opts.master_domid with
	| None ->
		debug "xc_domain_getinfo failed"
	| Some di ->
		debug "domain %d: dying = %b; shutdown = %b"
          di.OS.Domctl.domid di.OS.Domctl.dying di.OS.Domctl.shutdown
	end;

	while_lwt true do
		(*debug "tick";*)
		OS.Time.sleep 5.
	done

(* let _ = OS.Main.run (main ()) *)
