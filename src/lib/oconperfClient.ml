open OconperfConfiguration
open OconperfProtocol
open Printf
open Unix


let run () =
  let connected = ref true in
  if !quiet == false
  then print_endline (sprintf "Client connects to %s:%d" !addr !port)
  else ();
  let s = socket !socket_domain SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string !addr, !port)
  in
  connect s sa;
  while !connected do
    let (speed, latency) = client_run s ~max_time: (float_of_int !max_timeout) in
    print_endline (
      if !quiet == false
      then (sprintf "Download: %f ; Latency: %fs" speed latency)
      else (sprintf "%f\t%f" speed latency)
    );
  done;
  if !quiet == false
  then print_endline "Client disconnected."
  else ();
  0
