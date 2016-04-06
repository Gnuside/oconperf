open OconperfProtocolBase
open Unix;;
(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

Random.self_init ();

exception Result of cmd_t;;
exception Unexpected_answer of string;;

let rbuf_size = ref (OconperfProtocolBase.min_size)
and rbuf_i = ref 0;;
let rbuf = ref (Bytes.create OconperfProtocolBase.min_size);;

let random_buffer_size = 2*1024*1024;;
let random_buffer = Bytes.init random_buffer_size (fun _ -> char_of_int (Random.int 256))
and send_cmd fd cmd =
  let cmd_b = to_bytes cmd in
  if single_write fd cmd_b 0 (Bytes.length cmd_b) <> 0 then
    true
  else false
and recv_cmd fd =
  let min_read = ref OconperfProtocolBase.min_size in
  try
    while !rbuf_i < !min_read do
      try begin
        if !min_read > !rbuf_size then begin
          let n_buf = Bytes.create !min_read in
          Bytes.blit !rbuf 0 n_buf 0 !rbuf_i;
        rbuf := n_buf
        end;
        rbuf_i := !rbuf_i + (read fd !rbuf !rbuf_i !min_read);
        if !rbuf_i >= !min_read then begin
          let (cmd, buf) = of_bytes (Bytes.sub !rbuf 0 !rbuf_i) in
          rbuf := buf;
          rbuf_i := 0;
          rbuf_size := Bytes.length !rbuf;
          raise (Result(cmd))
        end
      end with Exn_read_more(_, mr) -> min_read := mr
    done;
    Answer(Read_failed)
  with Result(c) -> c
;;

(* compute average value from array *)
let average_l l =
  let len = List.length l
  and sum = ref 0.0 in
  List.iter (fun v -> sum := !sum +. v) l;
  !sum /. (float_of_int len)
;;

(* fill a bytes buffer with random data *)
let random_fill bytes =
  let len = Bytes.length bytes in
  let i = ref 0
  and l = ref (min len (Random.int random_buffer_size)) in
  while !i < len do
    let i_r = Random.int !l in
    Bytes.blit random_buffer i_r bytes !i (len - i_r);
    i := !i + (len - i_r);
    l := min (len - !i) (Random.int random_buffer_size)
  done
;;


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
      random_fill buf;
      ignore(send_cmd fd (Packet(buf)))
    end
    | answer -> raise (Unexpected_answer(cmd_to_string answer))
  done
;;
