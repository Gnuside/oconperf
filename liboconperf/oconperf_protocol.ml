open Oconperf_protocol_base
open Oconperf_pervasives
open Oconperf_list
open Oconperf_bytes
open Printf
open Unix;;
(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

Random.self_init ();

exception Result of cmd_t;;
exception Unexpected_answer of string;;
exception Unexpected_request of string;;
exception Invalid_request of string;;

let rbuf_size = ref (Oconperf_protocol_base.min_size)
and rbuf_i = ref 0;;
let rbuf = ref (Bytes.create !rbuf_size);;

(* Read and store data in rbuf, increase rbuf size if needed.
 * Does not update rbuf_i. *)
let rec recv_data fd offset min_read =
  let nbuf_size = offset + min_read in
  if nbuf_size > !rbuf_size then begin
    (* Allocate bigger buffer *)
    rbuf := Bytes.extend !rbuf 0 (nbuf_size - !rbuf_size);
    rbuf_size := nbuf_size;
    print_debug_f (fun () -> (sprintf "reallocate buffer: %d" nbuf_size));
  end;
  let r = read fd !rbuf offset min_read in
  if r >= min_read then begin
    r
  end else if r > 0 then begin
    (* Read more *)
    r + (recv_data fd (offset + r) (min_read - r))
  end else begin
    Unix.sleep 1;
    (* Try again *)
    recv_data fd offset min_read
  end
;;

let send_cmd fd cmd =
  let cmd_b = to_bytes cmd in
  print_debug_f (fun () -> (sprintf "send_cmd: %s..."
    (bytes_to_hex
      (Bytes.sub cmd_b 0 (min (Bytes.length cmd_b) (Oconperf_protocol_base.min_size + 8)))
      true)));
  (write fd cmd_b 0 (Bytes.length cmd_b)) <> 0
and recv_cmd fd =
  let min_read = ref Oconperf_protocol_base.min_size in
  try
    while !min_read > 0 do
      try begin
        let r = recv_data fd !rbuf_i !min_read in
        print_debug_f (fun () -> (sprintf "recv_cmd: %s..."
          (bytes_to_hex
            (Bytes.sub !rbuf 0 (min !rbuf_size (Oconperf_protocol_base.min_size + 8)))
            true)));
        rbuf_i := !rbuf_i + r;
        (* Raise Exn_read_more when we know the packet size *)
        let (cmd, _) = of_bytes !rbuf 0 !rbuf_i in
        print_debug_f (fun () -> sprintf "recv_cmd: %s..." (cmd_to_string cmd));
        rbuf_i := 0;
        raise (Result(cmd))
      end with Exn_read_more(_, mr) -> begin
        print_debug_f (fun () -> (sprintf "Read more %d..." mr));
        min_read := mr
      end
    done;
    Answer(Read_failed)
  with Result(c) -> c
;;

let client_download fd size =
  print_debug (Printf.sprintf "Server, please send %d bytes" size);
  let t0 = gettimeofday () in
  if send_cmd fd (Send size) then
    match recv_cmd fd with
    | Answer(Ok) -> begin
      let t1 = gettimeofday () in
      (* print_debug "Server say OK"; *)
      (* Then we continue *)
      match recv_cmd fd with
      | Packet(size') -> begin
        let t2 = gettimeofday () in
        if size == size'
        then begin
          print_message_f (fun () -> Printf.sprintf "I received %d data" size');
          (size, t2 -. t1, t1 -. t0)
        end else raise (Unexpected_answer("Size inconsistancy between Receive and Packet commands."));
      end
      | answer -> raise (Unexpected_answer(cmd_to_string answer))
    end
    | answer -> raise (Unexpected_answer(cmd_to_string answer))
  else
    failwith "Cannot send command"
  ;
and client_upload fd size =
  print_debug (Printf.sprintf "I send %d bytes to the server" size);
  let t0 = gettimeofday () in
  if send_cmd fd (Receive size) then
    match recv_cmd fd with
    | Answer(Ok) -> begin
      let t1 = gettimeofday () in
      (* print_debug "Server say OK"; *)
      (* Then we continue *)
      if send_cmd fd (Packet(size)) then
        let t2 = gettimeofday () in
        print_message_f (fun () -> Printf.sprintf "I sent %d data" size);
        (size, t2 -. t1, t1 -. t0)
      else
        failwith "Cannot send packet"
      ;
    end
    | answer -> raise (Unexpected_answer(cmd_to_string answer))
  else
    failwith "Cannot send command"
  ;
;;

(* Tant qu'on est en dessous de la moitié du max time, on fait grossir
 * la taille des paquets, ensuite on arrete de les faire grossir
 *
 * On arrete l'envoi/reception une fois le temps écoulé  *)

let client_run ?(test_upload=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) fd =
  let size = ref 256
  and start_time = gettimeofday ()
  and total_size = ref 0
  and total_time = ref 0.0
  and latencies = ref [] in
  begin try
    while (gettimeofday ()) -. start_time < max_time do
      let (s, t, latency) = if test_upload
                            then client_upload fd !size
                            else client_download fd !size
      and now = gettimeofday () in
      (* Collect data *)
      latencies := latency :: !latencies;
      total_size := !total_size + s;
      total_time := !total_time +. t;
      (* Maximum total size limit *)
      if max_size <> 0 && !total_size >= max_size then
        raise Exit
      ;
      (* Increase size value *)
      if (now -. start_time) *. 2. < max_time then
        size := !size * 2
      ;
      (* Maximum packet size limit *)
      if max_packet_size <> 0 && !size >= max_packet_size then
        size := min !size max_packet_size
      ;
    done;
    ignore(send_cmd fd Bye)
  with Exit -> ()
  end;
  (Some((float_of_int !total_size) /. !total_time), average_l !latencies)
and server_run fd =
  try
    while true do
      match recv_cmd fd with
      | Send(s) -> begin
        print_message_f (fun () -> Printf.sprintf "I saw a client asked %d bytes" s);
        (* send acknowledgement then a Packet command *)
        ignore(send_cmd fd (Answer(Ok)));
        ignore(send_cmd fd (Packet(s)))
      end
      | Receive(s) -> begin
        print_message_f (fun () -> Printf.sprintf "I saw a client will send %d bytes" s);
        ignore(send_cmd fd (Answer(Ok)));
        match recv_cmd fd with
        | Packet(s') -> begin
          if s == s'
          then ignore(send_cmd fd (Answer(Ok)))
          else raise (Unexpected_request("Size inconsistancy between Receive and Packet commands."))
        end
        | answer -> raise (Unexpected_request(cmd_to_string answer))
      end
      | Bye -> raise Exit
      | answer -> raise (Unexpected_request(cmd_to_string answer))
    done
  with
  | Unix_error(ECONNRESET, _, _)
  | Unix_error(EPIPE, _, _) -> begin
    print_error (sprintf "Write error (%s)" (error_message ECONNRESET))
  end
  | Unix_error(e, _, _) -> begin
    print_error (sprintf "Unix error (%s)" (error_message e))
  end
  | Exit -> ()
;;
