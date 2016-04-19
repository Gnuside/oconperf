open OconperfConfiguration
open OconperfProtocol
open OconperfPervasives
open Printf
open Unix

let connect_to addr port socket_domain ~iface =
  print_message_f (fun () -> (sprintf "Client connects to %s:%d" addr port));
  let s = socket socket_domain SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string addr, port) in
  begin
  match iface with
  | `Any -> () (* Any is the default behavior of socket,
                  and bind_to_interface needs root perms *)
  | _    -> Core.Std.Or_error.ok_exn Core.Linux_ext.bind_to_interface s iface
  end;
  connect s sa;
  s

let run () =
  let s = connect_to !addr !port !socket_domain ~iface: !iface in
  let (spd, lat) = (client_run s ~test_upload: !test_upload
                                 ~max_time: (float_of_int !max_timeout)
                                 ~max_size: !max_size)
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
