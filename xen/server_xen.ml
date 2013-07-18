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

let debug fmt = Logging.debug "server_xen" fmt
let warn  fmt = Logging.warn  "server_xen" fmt
let error fmt = Logging.error "server_xen" fmt

module DomainServer = Xs_server.Server(Xs_transport_domain)

let rec logging_thread logger =
	lwt lines = Logging.get logger in
	lwt () = Lwt_list.iter_s
			(fun x ->
				lwt () = OS.Console.log_s x in
				return ()
			) lines in
	logging_thread logger

let introduce_dom0 () =
	(* the cmd_line should have --event %d set by init-xenstore-domain.c *)
	let cmd_line = (OS.Start_info.((get ()).cmd_line)) in
	let bits = Junk.String.split ' ' cmd_line in
	let args = match bits with
	           | s :: _ -> Some (int_of_string s)
	           | _      -> None in
	match args with
	| None ->
		error "Failed to find control domain ID on commandline: %s" cmd_line;
		()
	| Some id ->
	  let mfn = (OS.Start_info.((get ()).store_mfn)) in
	  let port = (OS.Start_info.((get ()).store_evtchn)) in
		Introduce.(introduce { domid = id; mfn = (Nativeint.of_int mfn);
	                         remote_port = port });
		debug "Introduced domain %d with mfn = 0x%x, port = %d" id mfn port

let main () =
	debug "Mirage xenstored starting";
	let (_: 'a) = logging_thread Logging.logger in
	let (_: 'a) = logging_thread Logging.access_logger in

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
		debug "tick";
		OS.Time.sleep 5.
	done

(* let _ = OS.Main.run (main ()) *)
