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

let flask_operations: xssm_operations = {
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
  new_node_label = Hooks.new_node_label;
}

module DomainServer = Xs_server.Server(Xs_transport_domain)

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
			(fun x ->
				lwt () = OS.Console.log_s x in
				return ()
			) lines in
	logging_thread logger

(* Parsed command line options. *)
type options = {
  event: int option;
  master_domid: int option;
}

let introduce_dom0 () =
	(* the cmd_line should have --event %d set by init-xenstore-domain.c *)
	let cmd_line = (OS.Start_info.((get ()).cmd_line)) in
  debug "cmd_line: %s" cmd_line;
	let bits = Junk.String.split ' ' cmd_line in
  let rec loop opts = function
    | "--event" :: e :: rest ->
      loop { opts with event = Some (int_of_string e) } rest
    | "--master-domid" :: d :: rest ->
      loop { opts with master_domid = Some (int_of_string d) } rest
    | _ :: rest -> loop opts rest
    | [] -> opts in
  let opts = loop { event = None; master_domid = None } bits in
  match opts with
    | { event = Some port; master_domid = Some id } ->
	    let mfn = (OS.Start_info.((get ()).store_mfn)) in
      Introduce.(introduce { domid = id; mfn = Nativeint.of_int mfn;
                             remote_port = port });
		  debug "Introduced domain %d with mfn = 0x%x, port = %d" id mfn port
    | { event = None; _ } ->
      error "Missing --event option.";
      ()
    | { master_domid = None; _ } ->
      error "Missing --master-domid option.";
      ()

let main () =
	debug "Mirage xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in

	Xssm.set_security flask_operations;

	let (a: unit Lwt.t) = DomainServer.serve_forever () in
	debug "Started server on xen inter-domain transport";

	introduce_dom0 ();
(*
	debug "getdomaininfo 0";
	begin match OS.Domctl.getdomaininfo 0 with
	| None ->
		debug "xc_domain_getinfo failed"
	| Some di ->
		let open OS.Domctl.Xen_domctl_getdomaininfo in
		debug "domain %d: dying = %b; shutdown = %b" di.domid di.dying di.shutdown
	end;
*)
	while_lwt true do
		(*debug "tick";*)
		OS.Time.sleep 5.
	done

(* let _ = OS.Main.run (main ()) *)
