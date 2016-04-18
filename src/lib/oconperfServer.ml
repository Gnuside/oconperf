open OconperfConfiguration
open OconperfProtocol
open Printf
open Unix

let start () =
  let s = socket !socket_domain SOCK_STREAM 0
  in
    bind s (ADDR_INET(inet_addr_of_string !addr, !port)) ;
    listen s !mpc ;
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

let client_connection (fd, remote)  =
  print_endline (sprintf "  Connection from: %s" (string_of_sockaddr remote));
  server_run fd;
  client_disconnection (fd, remote);
  ()
;;

let sigint_handle socket =
  print_endline "Closing server...";
  close socket;
  exit 1
;;

let run () =
  print_endline (sprintf "Server (%s:%d)" !addr !port);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let s = start () in
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun n -> sigint_handle s));
  while true do
    print_endline "waiting connections...";
    let (fd, remote) = accept s
    in
    match fork() with
    | 0  -> client_connection (fd, remote); exit 0
    | id -> close fd
  done;
  close s;
  0
