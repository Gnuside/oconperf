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
let rbuf = ref (Bytes.create OconperfProtocolBase.min_size);;

let random_buffer_size = 2*1024*1024;;
let random_buffer = create_random_bytes random_buffer_size
and send_cmd fd cmd =
  let cmd_b = to_bytes cmd in
  print_endline (sprintf "send_cmd: %s" (bytes_to_hex cmd_b true));
  if single_write fd cmd_b 0 (Bytes.length cmd_b) <> 0
  then true
  else false
and recv_cmd fd =
  let min_read = ref OconperfProtocolBase.min_size in
  try
    while !min_read <> 0 do
      try begin
        if !min_read + !rbuf_i > !rbuf_size then begin
          let n_buf = Bytes.create (!min_read + !rbuf_i) in
          Bytes.blit !rbuf 0 n_buf 0 !rbuf_i;
        rbuf := n_buf
        end;
        print_endline (sprintf "recv_cmd: before rbuf_i:%d ; min_read:  %d" !rbuf_i !min_read);
        let r = read fd !rbuf !rbuf_i !min_read in
        print_endline (sprintf "recv_cmd: after rbuf_i:%d ; min_read: %d ; r: %d" !rbuf_i !min_read r);
        if r >= !min_read then begin
          rbuf_i := !rbuf_i + r;
          let (cmd, buf) = of_bytes (Bytes.sub !rbuf 0 !rbuf_i) in
          rbuf := buf;
          rbuf_i := 0;
          rbuf_size := Bytes.length !rbuf;
          print_endline (sprintf "recv_cmd: Result(%s) rbuf_i:%d ;min_read:  %d" (cmd_to_string cmd) !rbuf_i !min_read);
          raise (Result(cmd))
        end else begin
          rbuf_i := !rbuf_i + r;
          min_read := !min_read - r
        end
      end with Exn_read_more(_, mr) -> begin
        min_read := mr;
        print_endline (sprintf "recv_cmd: Exn rbuf_i:%d ;min_read:  %d" !rbuf_i !min_read)
      end
    done;
    Answer(Read_failed)
  with Result(c) -> c
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
    if send_cmd fd (Send(!size)) then
      match recv_cmd fd with
      | Answer(Ok) -> begin
        (* Then we continue *)
        match recv_cmd fd with
        | Packet(s) -> begin
          let now = gettimeofday () in
          print_endline (
            Printf.sprintf "I received %d data !" (Bytes.length s));
          if (now -. start_time) *. 2. < max_time then
            size := !size * 2
        end
        | answer -> raise (Unexpected_answer(cmd_to_string answer))
      end
      | answer -> raise (Unexpected_answer(cmd_to_string answer))
  done;
  (average_l !speeds, average_l !latencies)
and server_run fd =
  while true do
    match recv_cmd fd with
    | Send(s) -> begin
      (* send acknowledgement then a Packet command *)
      ignore(send_cmd fd (Answer(Ok)));
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
