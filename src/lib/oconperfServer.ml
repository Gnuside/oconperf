open OconperfConfiguration
open Printf
open Unix

let start () =
  let s4 = socket PF_INET SOCK_STREAM 0
  and s6 = socket PF_INET6 SOCK_STREAM 0
  in
    bind s4 (ADDR_INET(inet_addr_of_string !addr4, !port));
    bind s6 (ADDR_INET(inet_addr_of_string !addr6, !port));
    listen s4 !mpc;
    listen s6 !mpc;
    (s4,s6)
and close (s4,s6) =
  Unix.close s4;
  Unix.close s6
;;

let string_of_sockaddr sa =
  match sa with
  | ADDR_INET(a, p) -> sprintf "%s:%d" (string_of_inet_addr a) p
  | ADDR_UNIX(path) -> sprintf "Unix://%s" path
;;

let run_connection (fd,remote)  =
  let buffer = Bytes.of_string "Hello"
  in
  print_endline (sprintf "  Connection from: %s" (string_of_sockaddr remote));
  ignore(send fd buffer 0 5 []);
  shutdown fd SHUTDOWN_ALL;
  ()
;;

let run () =
  print_endline (sprintf "Server (%s:%d)" !addr4 !port);
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let (s4,s6) = start () in
  while true do
    print_endline "waiting connections...";
    run_connection (accept s4)
  done;
  0
