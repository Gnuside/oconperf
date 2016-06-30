open Oconperf_bytes
open Oconperf_pervasives
open Printf
(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

type error_t =
  | Ok
  | Parsing
  | Cmd
  | Read_failed
  | Too_big

type header_t = int * int

type cmd_t =
  | Send of int (* command asking to the server to send data *)
  | Receive of int (* command asking to the server to receive data *)
  | Packet of int (* Command containing data *)
  | Answer of error_t
  | Bye

let cmd_values = [
  0x01 ; (* Send *)
  0x02 ; (* Receive *)
  0x03 ; (* Packet *)
  0x04 ; (* Answer *)
  0xFF ; (* Bye *)
]
(*
and err_values = [
  0x00 ; (* OK *)
  0x01 ; (* Parsing error *)
  0x02 ; (* Cmd error *)
  0x03 ; (* Read failed error *)
  0x04 ; (* Too big error *)
] *)

let is_valid_command_code b = List.exists (fun i -> i = b) cmd_values
and min_size = 1 + 4

let random_buffer_size = 2*1024*1024
let random_buffer = create_random_bytes random_buffer_size

exception Exn_invalid_command of int
exception Exn_invalid_error_code of char
exception Exn_invalid_length of int * int
exception Exn_invalid_digest of Digest.t * Digest.t
exception Exn_uncoherent_cmd of string * bytes
exception Exn_read_more of bytes * int (* (buf,r) : read r more bytes for buf to be complete *)

let err_to_string = function
  | Ok          -> "Ok"
  | Parsing     -> "Parsing"
  | Cmd         -> "Cmd"
  | Read_failed -> "Read_failed"
  | Too_big     -> "Too_big"


let cmd_to_string = function
  | Send(l)     -> sprintf "Send(%d)" l
  | Receive(l)  -> sprintf "Receive(%d)" l
  | Answer(err) -> sprintf "Answer(%s)" (err_to_string err)
  | Packet(len) -> sprintf "Packet(%d)" len
  | Bye         -> "Bye"


(* forge : get ocaml value from bytes stream
 * unforge : put ocaml value into bytes stream
 *
 * Values are put in BigEndian representation.
 *)
(* 32 bits integers *)
let forge_uint32 buffer offset =
  (* doens't work on 32 bits architectures *)
  ((int_of_char (Bytes.get buffer offset)) lsl 24) lor
  ((int_of_char (Bytes.get buffer (offset + 1))) lsl 16) lor
  ((int_of_char (Bytes.get buffer (offset + 2))) lsl  8) lor
  ((int_of_char (Bytes.get buffer (offset + 3))) lsl  0)
and unforge_uint32 u32 =
  let f i =
    char_of_int ((u32 lsr (8*(3-i))) land 0xFF)
  in Bytes.init 4 f
(* Error codes *)
and forge_err buffer offset =
  match Bytes.get buffer offset with
  | '\x00' -> Ok
  | '\x01' -> Parsing
  | '\x02' -> Cmd
  | '\x03' -> Read_failed
  | '\x04' -> Too_big
  | err    -> raise (Exn_invalid_error_code(err))
and unforge_err = function
  | Ok          -> Bytes.make 1 '\x00'
  | Parsing     -> Bytes.make 1 '\x01'
  | Cmd         -> Bytes.make 1 '\x02'
  | Read_failed -> Bytes.make 1 '\x03'
  | Too_big     -> Bytes.make 1 '\x04'


(* command must be of the form :
  * <code:1> <length:4> <data:length> <digest:16>
  *
  * - code : one of the command codes (in cmd_values)
  * - length : 4 bytes unsigned integer designating the payload length
  * - data : payload
  * - digest : MD5 digest of `data`
  *
  *)
let forge_cmd cmd data offset len =
  if not (is_valid_command_code cmd) then raise (Exn_invalid_command(cmd));
  match (cmd, len) with
  | (0x01, 4) -> Send(forge_uint32 data offset)
  | (0x01, _) -> raise (Exn_uncoherent_cmd("Send", data))
  | (0x02, 4) -> Receive(forge_uint32 data offset)
  | (0x02, _) -> raise (Exn_uncoherent_cmd("Receive", data))
  | (0x03, _) -> Packet(len)
  | (0x04, 1) -> Answer(forge_err data offset)
  | (0x04, _) -> raise (Exn_uncoherent_cmd("Answer", data))
  | (0xFF, 0) -> Bye
  | (0xFF, _) -> raise (Exn_uncoherent_cmd("Bye", data))
  | (_,_) -> raise (Exn_uncoherent_cmd(sprintf "Unknown : cmd:%d len:%d" cmd len, data))
and unforge_cmd = function
  | Send(l) -> begin
    let data = unforge_uint32 l in
    (Bytes.cat
      (Bytes.of_string "\x01")
      (Bytes.cat
        (unforge_uint32 4)
        data
      )
    )
  end
  | Receive(l) -> begin
    let data = unforge_uint32 l in
    (Bytes.cat
      (Bytes.of_string "\x02")
      (Bytes.cat
        (unforge_uint32 4)
        data
      )
    )
  end
  | Packet(len) -> begin
    let data = Bytes.create len in
    random_fill data len random_buffer random_buffer_size;
    (Bytes.cat
      (Bytes.of_string "\x03")
      (Bytes.cat
        (unforge_uint32 len)
        data
      )
    )
  end
  | Answer(a) -> begin
    let data = unforge_err a in
    (Bytes.cat
      (Bytes.of_string "\x04")
      (Bytes.cat
        (unforge_uint32 1)
        data
      )
    )
  end
  | Bye -> begin
    (Bytes.cat
      (Bytes.of_string "\xFF")
      (unforge_uint32 0)
    )
  end


let of_bytes_header buffer offset buf_len =
  let header_size = min_size
  and len = buf_len - offset in
  if len < header_size
  then None
  else begin
    let cmd = int_of_char (Bytes.get buffer offset)
    and data_len = forge_uint32 buffer (offset + 1) in
    Some (cmd, data_len)
  end


let of_bytes_body buffer offset buf_len (cmd, data_len) =
  let len = buf_len - offset in
  begin if len < data_len then
    None
  else
    Some (forge_cmd cmd buffer offset data_len)
  end

let of_bytes buffer offset buf_len =
  let header_size = min_size
  and len = buf_len - offset in
  match (of_bytes_header buffer offset buf_len) with
  | Some(cmd, data_len) -> begin
    let opt = (of_bytes_body buffer (offset + header_size) buf_len (cmd, data_len))
    in (opt, data_len - len)
  end
  | None -> (None, header_size - len)


let to_bytes cmd = unforge_cmd cmd

