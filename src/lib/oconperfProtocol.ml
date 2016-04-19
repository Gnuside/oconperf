open OconperfProtocolBase
open OconperfPervasives
open OconperfList
open OconperfBytes
open Printf
open Unix;;
(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

Random.self_init ();

exception Result of cmd_t;;
exception Unexpected_answer of string;;
exception Unexpected_request of string;;
exception Invalid_request of string;;

let rbuf_size = ref (OconperfProtocolBase.min_size)
and rbuf_i = ref 0;;
let rbuf = ref (Bytes.create !rbuf_size);;

let random_buffer_size = 2*1024*1024;;
let random_buffer = create_random_bytes random_buffer_size;;

let rec write_cmd fd cmd_b len =
  try begin
    write fd cmd_b 0 len
  end with Unix_error(e, _, _) -> begin
    print_error (sprintf "Write error (%s), retrying..." (error_message e));
    Unix.sleep 1;
    write_cmd fd cmd_b len
  end
;;

let send_cmd fd cmd =
  let cmd_b = to_bytes cmd in
  print_debug (sprintf "send_cmd: %s..."
    (bytes_to_hex
      (Bytes.sub cmd_b 0 (min (Bytes.length cmd_b) OconperfProtocolBase.min_size))
      true));
  (write_cmd fd cmd_b (Bytes.length cmd_b)) <> 0
and recv_cmd fd =
  let min_read = ref OconperfProtocolBase.min_size in
  print_debug (sprintf "recv_cmd: min_read: %d" !min_read);
  try
    while !min_read > 0 do
      try begin
        if !min_read + !rbuf_i > !rbuf_size then begin
          let n_buf = Bytes.create (!min_read + !rbuf_i) in
          Bytes.blit !rbuf 0 n_buf 0 !rbuf_i;
          rbuf := n_buf
        end;
        (* print_debug (sprintf "recv_cmd: before rbuf_i: %d ; min_read: %d" !rbuf_i !min_read); *)
        let r = read fd !rbuf !rbuf_i !min_read in
        (* print_debug (sprintf "recv_cmd: after rbuf_i: %d ; min_read: %d ; r: %d" !rbuf_i !min_read r); *)
        if r >= !min_read then begin
          rbuf_i := !rbuf_i + r;
          let (cmd, buf) = of_bytes (Bytes.sub !rbuf 0 !rbuf_i) in
          rbuf := buf;
          rbuf_i := 0;
          rbuf_size := Bytes.length !rbuf;
          print_debug (sprintf "recv_cmd: Result(%s) rbuf_i: %d ; min_read: %d" (cmd_to_string cmd) !rbuf_i !min_read);
          raise (Result(cmd))
        end else if r > 0 then begin
          print_debug (sprintf "recv_cmd: before r: %d ; rbuf_i: %d ; min_read: %d" r !rbuf_i !min_read);
          rbuf_i := !rbuf_i + r;
          min_read := !min_read - r;
          print_debug (sprintf "recv_cmd: after r: %d ; rbuf_i: %d ; min_read: %d" r !rbuf_i !min_read)
        end else if r < 0 then begin
          print_error (sprintf "error read %d" r)
        end else begin
          Unix.sleep 1
        end
      end with Exn_read_more(_, mr) -> begin
        min_read := mr;
        print_debug (sprintf "recv_cmd: Exn rbuf_i: %d ; min_read: %d" !rbuf_i !min_read)
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
      print_debug "Server say OK";
      (* Then we continue *)
      match recv_cmd fd with
      | Packet(s) -> begin
        let t2 = gettimeofday () in
        print_message (Printf.sprintf "I received %d data" (Bytes.length s));
        ((float_of_int size) /. (t2 -. t1), t1 -. t0)
      end
      | answer -> raise (Unexpected_answer(cmd_to_string answer))
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

let client_run ?(max_time=2.0) fd =
  let size = ref 256
  and start_time = gettimeofday ()
  and speeds = ref []
  and latencies = ref [] in
  while (gettimeofday ()) -. start_time < max_time do
    let (speed, latency) = client_download fd !size
    and now = gettimeofday () in
    speeds := speed :: !speeds;
    latencies := latency :: !latencies;
    if (now -. start_time) *. 2. < max_time then
      size := !size * 2
    ;
  done;
  (average_l !speeds, average_l !latencies)
and server_run fd =
  while true do
    match recv_cmd fd with
    | Send(s) -> begin
      print_message (Printf.sprintf "I saw you asked %d bytes" s);
      (* send acknowledgement then a Packet command *)
      ignore(send_cmd fd (Answer(Ok)));
      print_message (Printf.sprintf "I prepare %d bytes of random data..." s);
      let buf = Bytes.create s in
      random_fill buf random_buffer random_buffer_size;
      ignore(send_cmd fd (Packet(buf)))
    end
    | Receive(s) -> begin
      ignore(send_cmd fd (Answer(Ok)));
      match recv_cmd fd with
      | Packet(b) -> begin
        if s == (Bytes.length b)
        then ignore(send_cmd fd (Answer(Ok)))
        else raise (Unexpected_request("Size inconsistancy between Receive and Packet commands."))
      end
      | answer -> raise (Unexpected_request(cmd_to_string answer))
    end
    | answer -> raise (Unexpected_request(cmd_to_string answer))
  done
;;
