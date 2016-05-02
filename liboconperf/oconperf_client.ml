open Oconperf_configuration
open Oconperf_protocol
open Oconperf_pervasives
open Printf
open Unix
open Core

let connect_to ~iface ~max_time addr port =
  let inet_addr =
    gethostbyname addr
    |> fun x -> x.h_addr_list.(0)
  in

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


let speed_test ?(test_upload=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) ?(iface=`Any) addr port =
  let s = connect_to addr port ~iface: iface ~max_time: max_time in
  client_run s ~test_upload: test_upload
               ~max_time: max_time
               ~max_size: max_size
               ~max_packet_size: max_packet_size


let run ?(test_upload=false) ?(human_readable=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) ?(iface=`Any) addr port =
  try begin
    let (spd, lat) =
      speed_test ~test_upload: test_upload
        ~max_time: max_time
        ~max_size: max_size
        ~max_packet_size: max_packet_size
        ~iface: iface
        addr
        port
    in

    match spd, lat with
    | Some(speed), Some(latency) -> begin
      let speed_with_unit =
        if human_readable
        then show_bytes_human_readable Binary_power speed
        else sprintf "%d" (int_of_float speed)
      in let speed_with_unit_always =
        let out = speed_with_unit in
        if human_readable
        then out
        else sprintf "%s iB / s" out
      in
      print_endline (
        if !quiet == false
        then (sprintf "Speed: %s ; Latency: %f s" speed_with_unit_always latency)
        else (sprintf "%s\t%f" speed_with_unit latency)
      );
      print_message "Client disconnected.";
      0
    end
    | _, _ -> print_error "Cannot compute speed and latency"; 1
  end with e -> begin
    print_error (sprintf "Unknown error (%s)" (Printexc.to_string e)); 1
  end
