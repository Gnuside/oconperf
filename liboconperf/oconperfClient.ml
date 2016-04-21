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

let bytes_unit_to_string = function
| 0 -> ""
| 1 -> "K"
| 2 -> "M"
| 3 -> "G"
| 4 -> "T"
| 5 -> "P"
| 6 -> "E"
| 7 -> "Z"
| _ -> "Y"

let show_bytes_human_readable a = function
| 0. -> "0 B"
| v -> begin
  let i = min (max (floor ((log v) /. (log a))) 0.) 8. in
  sprintf "%.2f %sB / s" (v /. (a ** i)) (bytes_unit_to_string (int_of_float i))
end

let show_bytes_str v =
  if !human_readable then
    show_bytes_human_readable 1024. v
  else
    sprintf "%f" v
;;

let run () =
  let s = connect_to !addr !port !socket_domain ~iface: !iface in
  let (spd, lat) = (client_run s ~test_upload: !test_upload
                                 ~max_time: (float_of_int !max_timeout)
                                 ~max_size: !max_size
                                 ~max_packet_size: !max_packet_size)
  in match spd, lat with
  | Some(speed), Some(latency) -> begin
    print_endline (
      if !quiet == false
      then (sprintf "Download: %s ; Latency: %f s" (show_bytes_str speed) latency)
      else (sprintf "%f\t%f" speed latency)
    );
    print_message "Client disconnected.";
    0
  end
  | _, _ -> print_error "Cannot compute speed and latency"; 1
