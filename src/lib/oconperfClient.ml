open OconperfConfiguration
open OconperfProtocol
open OconperfPervasives
open Printf
open Unix
open Core.Std

let connect_to addr port socket_domain =
  print_message (sprintf "Client connects to %s:%d" addr port);
  let s = socket socket_domain SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string addr, port)
  in
  let open Core.Linux_ext in
  let toto (fd:Unix.File_descr.t) truc =
     Core.Linux_ext.bind_to_interface fd truc ;
  in
  ignore @@ toto s  (`Interface_name "eth0")
  connect s sa;
  s

let run () =
  let s = connect_to !addr !port !socket_domain in
  let (spd, lat) = client_run s ~max_time: (float_of_int !max_timeout)
  in match spd, lat with
  | Some(speed), Some(latency) -> begin
    print_endline (
      if !quiet == false
      then (sprintf "Download: %f ; Latency: %fs" speed latency)
      else (sprintf "%f\t%f" speed latency)
    );
    print_message "Client disconnected.";
    0
  end
  | _, _ -> print_error "Cannot compute speed and latency"; 1
