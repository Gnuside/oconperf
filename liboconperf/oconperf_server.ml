open Oconperf_protocol
open Printf
open Unix

let start ~max_pending_request addr port =
  let inet_addr = inet_addr_of_string addr in
  let sa = ADDR_INET(inet_addr, port) in
  let s = socket (domain_of_sockaddr sa) SOCK_STREAM 0 in
  bind s sa ;
  listen s max_pending_request ;
  s
and close s =
  Unix.close s
and shutdown fd =
  Unix.shutdown fd SHUTDOWN_ALL
;;

let string_of_sockaddr sa =
  match sa with
  | ADDR_INET(a, p) -> sprintf "%s:%d" (string_of_inet_addr a) p
  | ADDR_UNIX(path) -> sprintf "Unix://%s" path
;;

let client_disconnection (fd, remote) =
  print_endline (sprintf "  Client %s leaving..." (string_of_sockaddr remote));
  shutdown fd;
  ()
;;

let client_connection ?(max_packet_size=0) (fd, remote) =
  print_endline (sprintf "  Connection from: %s" (string_of_sockaddr remote));
  server_run fd ~max_packet_size: max_packet_size;
  client_disconnection (fd, remote);
  ()
;;

let sigint_handle socket =
  print_endline "Closing server...";
  close socket;
  exit 1
;;

let run ?(max_packet_size=0) ~max_pending_request addr port =
  print_endline (sprintf "Server (%s:%d)" addr port);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let s = start addr port ~max_pending_request: max_pending_request in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> sigint_handle s));
  while true do
    print_endline "waiting connections...";
    let (fd, remote) = accept s
    in
    match fork() with
    | 0  -> begin
      if Unix.fork() <> 0 then exit 0;
      try
        client_connection (fd, remote) ~max_packet_size: max_packet_size; exit 0
      with _ -> begin
        client_disconnection (fd, remote);
        exit 1
      end
    end
    | id -> close fd; ignore(Unix.waitpid [] id)
  done;
  close s;
  0
