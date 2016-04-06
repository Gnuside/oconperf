open OconperfConfiguration
open OconperfProtocol
open Printf
open Unix


let run () =
  let connected = ref true in
  print_endline (sprintf "Client connects to %s:%d" !addr4 !port);
  let s4 = socket PF_INET SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string !addr4, !port)
  in
  connect s4 sa;
  while !connected do
    let (speed, latency) = client_run s4 in
    print_endline (sprintf "Download: %f ; Latency: %fs" speed latency)
  done;
  print_endline "Client disconnected.";
  0
