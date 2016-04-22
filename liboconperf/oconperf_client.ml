open Oconperf_configuration
open Oconperf_protocol
open Oconperf_pervasives
open Printf
open Unix

let connect_to ~iface ~max_time addr port =
  let inet_addr = inet_addr_of_string addr in
  print_message_f (fun () -> (sprintf "Client connects to %s:%d" addr port));
  let sa = ADDR_INET(inet_addr, port) in
  let s = socket (domain_of_sockaddr sa) SOCK_STREAM 0 in
  begin
  match iface with
  | `Any -> () (* Any is the default behavior of socket,
                  and bind_to_interface needs root perms *)
  | _    -> Core.Std.Or_error.ok_exn Core.Linux_ext.bind_to_interface s iface
  end;
  (* Set timeout *)
  if max_time <> 0.0 then begin
    setsockopt_float s SO_RCVTIMEO max_time;
    setsockopt_float s SO_SNDTIMEO max_time
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
| 0. -> "0 iB"
| v -> begin
  let i = min (max (floor ((log v) /. (log a))) 0.) 8. in
  sprintf "%.2f %siB / s" (v /. (a ** i)) (bytes_unit_to_string (int_of_float i))
end

let speed_test ?(test_upload=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) ?(iface=`Any) addr port =
  let s = connect_to addr port ~iface: iface ~max_time: max_time in
  client_run s ~test_upload: test_upload
               ~max_time: max_time
               ~max_size: max_size
               ~max_packet_size: max_packet_size

let run ?(test_upload=false) ?(human_readable=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) ?(iface=`Any) addr port =
  try begin
    let (spd, lat) = speed_test ~test_upload: test_upload
                                ~max_time: max_time
                                ~max_size: max_size
                                ~max_packet_size: max_packet_size
                                ~iface: iface
                                addr
                                port
    in match spd, lat with
    | Some(speed), Some(latency) -> begin
      let speed_with_unit = if human_readable
                            then show_bytes_human_readable 1024. speed
                            else sprintf "%f" speed
      in
      print_endline (
        if !quiet == false
        then (sprintf "Download: %s ; Latency: %f s" speed_with_unit latency)
        else (sprintf "%s\t%f" speed_with_unit latency)
      );
      print_message "Client disconnected.";
      0
    end
    | _, _ -> print_error "Cannot compute speed and latency"; 1
  end with e -> begin
    print_error (sprintf "Unknown error (%s)" (Printexc.to_string e)); 1
  end
