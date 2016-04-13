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
and close (s) =
  Unix.close s
;;

let string_of_sockaddr sa =
  match sa with
  | ADDR_INET(a, p) -> sprintf "%s:%d" (string_of_inet_addr a) p
  | ADDR_UNIX(path) -> sprintf "Unix://%s" path
;;

let run_connection (fd,remote)  =
  print_endline (sprintf "  Connection from: %s" (string_of_sockaddr remote));
  server_run fd;
  shutdown fd SHUTDOWN_ALL;
  ()
;;

let run () =
  print_endline (sprintf "Server (%s:%d)" !addr !port);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (s) = start () in
  while true do
    print_endline "waiting connections...";
    run_connection (accept s)
  done;
  0
