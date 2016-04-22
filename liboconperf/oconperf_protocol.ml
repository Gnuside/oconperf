open Oconperf_protocol_base
open Oconperf_pervasives
open Oconperf_list
open Oconperf_bytes
open Printf
open Unix;;
(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

Random.self_init ();

exception Unexpected_answer of cmd_t;;
exception Invalid_answer of string;;
exception Unexpected_request of cmd_t;;
exception Invalid_request of string;;
exception Cannot_send of cmd_t;;

let rbuf_size = ref (Oconperf_protocol_base.min_size * 2);;
let rbuf = ref (Bytes.create !rbuf_size);;

(* Read and store data in rbuf, increase rbuf size if needed. *)
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
    print_debug_f (fun () -> "recv_data: wait and try again...");
    ignore (select [fd] [] [] (-1.));
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
and recv_cmd fd max_packet_size =
  let max_read = if max_packet_size <> 0
                 then Oconperf_protocol_base.min_size + max_packet_size
                 else 0
  in
  let recv_body offset (cmd_num, data_len) =
    if max_read <> 0 && data_len > max_read then begin
      print_debug_f (fun () -> (sprintf "Cowardly refuse packets with size like %d B" data_len));
      Answer(Too_big)
    end else begin
      let r = recv_data fd offset data_len in
      print_debug_f (fun () -> (sprintf "recv_body: %s..."
         (bytes_to_hex
           (Bytes.sub !rbuf offset (min (!rbuf_size - offset) (Oconperf_protocol_base.min_size + 8)))
           true)));
      match of_bytes_body !rbuf offset (offset + r) (cmd_num, data_len) with
      | Some(cmd) -> begin
        print_debug_f (fun () -> sprintf "recv_body: %s" (cmd_to_string cmd));
        cmd
      end
      | None -> begin
        failwith "recv_body: There wasn't enough bytes asked, this should not happen"
      end
    end
  in let recv_header () =
    let r = recv_data fd 0 Oconperf_protocol_base.min_size in
    print_debug_f (fun () -> (sprintf "recv_header: %s" (bytes_to_hex
      (Bytes.sub !rbuf 0 r)
      true)));
    match of_bytes_header !rbuf 0 r with
    | Some(cmd_num, data_len) -> begin
      recv_body r (cmd_num, data_len)
    end
    | None -> begin
      failwith "recv_header: There wasn't enough bytes asked, this should not happen"
    end
  in recv_header ()
;;

let client_download fd size max_packet_size =
  print_debug (sprintf "Server, please send %d bytes" size);
  let t0 = gettimeofday () in
  if send_cmd fd (Send size) then
    match recv_cmd fd max_packet_size with
    | Answer(Ok) -> begin
      let t1 = gettimeofday () in
      (* print_debug "Server say OK"; *)
      (* Then we continue *)
      match recv_cmd fd max_packet_size with
      | Packet(size') -> begin
        let t2 = gettimeofday () in
        if size == size'
        then begin
          let packet_size = Oconperf_protocol_base.min_size + size in
          print_debug_f (fun () -> sprintf "I received %d data" size);
          (packet_size, t2 -. t1, t1 -. t0)
        end else
          raise (Invalid_answer("Size inconsistancy between Receive and Packet commands."))
        ;
      end
      | answer -> raise (Unexpected_answer(answer))
    end
    | answer -> raise (Unexpected_answer(answer))
  else
    raise (Cannot_send(Send size))
  ;
and client_upload fd size max_packet_size =
  print_debug_f (fun () -> (Printf.sprintf "I send %d bytes to the server" size));
  let t0 = gettimeofday () in
  if send_cmd fd (Receive size) then
    match recv_cmd fd max_packet_size with
    | Answer(Ok) -> begin
      let t1 = gettimeofday () in
      (* print_debug "Server say OK"; *)
      (* Then we continue *)
      if send_cmd fd (Packet size) then
        let t2 = gettimeofday ()
        and packet_size = Oconperf_protocol_base.min_size + size in
        print_debug_f (fun () -> sprintf "I sent %d data" size);
        (packet_size, t2 -. t1, t1 -. t0)
      else
        raise (Cannot_send(Packet size))
      ;
    end
    | answer -> raise (Unexpected_answer(answer))
  else
    raise (Cannot_send(Receive size))
  ;
;;

(* Tant qu'on est en dessous de la moitié du max time, on fait grossir
 * la taille des paquets, ensuite on arrete de les faire grossir
 *
 * On arrete l'envoi/reception une fois le temps écoulé  *)

let in_time start_time max_time =
  (gettimeofday ()) -. start_time < max_time
and remaining_time max_time =
  max_time -. (gettimeofday ())

let client_run ?(test_upload=false) ?(max_time=2.0) ?(max_size=0) ?(max_packet_size=0) fd =
  let size = ref 256
  and start_time = gettimeofday ()
  and total_size = ref 0
  and total_time = ref 0.0
  and latencies = ref [] in
  print_message "Please wait...";
  begin try
    while in_time start_time max_time do
      let (read_fd, write_fd) = pipe () in
      let input = Unix.in_channel_of_descr read_fd
      and output = Unix.out_channel_of_descr write_fd
      in
      let _run_child () =
        close read_fd;
        let (s, t, latency) = if test_upload
                              then client_upload fd !size max_packet_size
                              else client_download fd !size max_packet_size
        in
        (* Send response to output *)
        Marshal.to_channel output (s, t, latency) [Marshal.No_sharing];
        flush output;
        exit 0
      and _run_parent pid =
        close write_fd;
        let collect_data (s, t, latency) =
          let now = gettimeofday () in
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
        in let wait_until_child_writes () =
          if in_time start_time max_time then begin
            (* FIXME: timeout imprecisions *)
            ignore (select [read_fd] [] [] (remaining_time max_time));
            collect_data (Marshal.from_channel input);
            ignore (waitpid [] pid);
          end; ()
        in wait_until_child_writes ();
        close read_fd
      in match fork() with
      | 0   -> _run_child ()
      | pid -> _run_parent pid
    done;
    ignore(send_cmd fd Bye)
  with
  | Unix_error(e, _, _) -> begin
    print_error (sprintf "Unix error (%s)" (error_message e))
  end
  | Cannot_send(cmd) -> begin
    print_error (sprintf "Unable to send command to the server: %s" (cmd_to_string cmd))
  end
  | Invalid_answer(s) -> begin
    print_error (sprintf "Invalid answer from the server: %s" s)
  end
  | Unexpected_answer(cmd) -> begin
    print_error (sprintf "Unexpected answer from the server: %s" (cmd_to_string cmd))
  end
  | Exit -> begin
    print_error "Exit..."
  end
  end;
  (Some((float_of_int !total_size) /. !total_time), average_l !latencies)
and server_run ?(max_packet_size=0) fd =
  try
    while true do
      print_message "Wait for client request...";
      match recv_cmd fd max_packet_size with
      | Send(s) -> begin
        print_message_f (fun () -> Printf.sprintf "I saw a client asked %d bytes" s);
        let my_answer = (if max_packet_size <> 0 && s > max_packet_size
                        then Answer(Too_big)
                        else Answer(Ok)) in
        print_message_f (fun () -> Printf.sprintf "My answer: %s" (cmd_to_string my_answer));
        (* send acknowledgement then a Packet command *)
        if not (send_cmd fd my_answer) then
          raise (Cannot_send my_answer)
        ;
        if my_answer = Answer(Ok) && not (send_cmd fd (Packet s)) then
          raise (Cannot_send (Packet s))
        ;
      end
      | Receive(s) -> begin
        print_message_f (fun () -> Printf.sprintf "I saw a client will send %d bytes" s);
        let my_answer = (if max_packet_size <> 0 && s > max_packet_size
                        then Answer(Too_big)
                        else Answer(Ok)) in
        print_message_f (fun () -> Printf.sprintf "My answer: %s" (cmd_to_string my_answer));
        if not (send_cmd fd my_answer) then
          raise (Cannot_send my_answer)
        ;
        if my_answer = Answer(Ok) then
          match recv_cmd fd max_packet_size with
          | Packet(s') -> begin
            if s == s'
            then begin
              if not (send_cmd fd (Answer(Ok))) then
                raise (Cannot_send (Answer Ok))
              ;
            end else raise (Invalid_request("Size inconsistancy between Receive and Packet commands."))
          end
          | answer -> raise (Unexpected_request(answer))
        ;
      end
      | Bye -> raise Exit
      | answer -> raise (Unexpected_request(answer))
    done
  with
  | Unix_error(ECONNRESET, _, _)
  | Unix_error(EPIPE, _, _) -> begin
    print_error (sprintf "Write error (%s)" (error_message ECONNRESET))
  end
  | Unix_error(e, _, _) -> begin
    print_error (sprintf "Unix error (%s)" (error_message e))
  end
  | Cannot_send(cmd) -> begin
    print_error (sprintf "Unable to send command to the client: %s" (cmd_to_string cmd))
  end
  | Invalid_request(s) -> begin
    print_error (sprintf "Request invalid (%s)" s);
    ignore(send_cmd fd Bye)
  end
  | Unexpected_request(cmd) -> begin
    print_error (sprintf "Request error (%s)" (cmd_to_string cmd));
    ignore(send_cmd fd Bye)
  end
  | Exit -> ()
;;
