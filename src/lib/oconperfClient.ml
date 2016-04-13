open OconperfConfiguration
open OconperfProtocol
open Printf
open Unix


let run () =
  let connected = ref true in
  print_endline (sprintf "Client connects to %s:%d" !addr !port);
  let s = socket !socket_domain SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string !addr, !port)
  in
  connect s sa;
  while !connected do
    let (speed, latency) = client_run s ~max_time: (float_of_int !max_timeout) in
    print_endline (sprintf "Download: %f ; Latency: %fs" speed latency)
  done;
  print_endline "Client disconnected.";
  0
