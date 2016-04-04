(* Client connects, then ask server to send (Send) to the client data
 * or to receive (Receive) data from the client *)

type error_t =
  | Ok
  | Parsing
  | Cmd

type cmd_t =
  | Send of int (* command asking to the server to send data *)
  | Receive of int (* command asking to the server to receive data *)
  | Packet of bytes (* Command containing data *)
  | Answer of error_t

let cmd_values = [
  0x01 ; (* Send *)
  0x02 ; (* Receive *)
  0x03 ; (* Packet *)
  0x04 (* Answer *)
]
and err_values = [
  0x00 ; (* OK *)
  0x01 ; (* Parsing error *)
  0x02 ; (* Cmd error *)
];;

let is_valid_command_code b = List.exists (fun i -> i = b) cmd_values
and min_size = 1 + 5 + 8;;


exception Exn_invalid_command of int
exception Exn_invalid_error_code of char
exception Exn_invalid_length of int * int
exception Exn_invalid_digest of Digest.t * Digest.t
exception Exn_uncoherent_cmd of string * bytes
exception Exn_read_more of bytes * int (* (buf,r) : read r more bytes for buf to be complete *)

(* forge : get ocaml value from bytes stream
 * unforge : put ocaml value into bytes stream
 *
 * Values are put in BigEndian representation.
 *)
(* 32 bits integers *)
let forge_uint32 buffer =
  (* doens't work on 32 bits architectures *)
  ((int_of_char (Bytes.get buffer 0)) lsl 24) lor
  ((int_of_char (Bytes.get buffer 1)) lsl 16) lor
  ((int_of_char (Bytes.get buffer 2)) lsl  8) lor
  ((int_of_char (Bytes.get buffer 3)) lsl  0)
and unforge_uint32 u32 =
  let f i =
    char_of_int ((u32 lsr (8*(3-i))) land 0xFF)
  in Bytes.init 4 f
(* Error codes *)
and forge_err buffer =
  match Bytes.get buffer 0 with
  | '\x00' -> Ok
  | '\x01' -> Parsing
  | '\x02' -> Cmd
  | err -> raise (Exn_invalid_error_code(err))
and unforge_err = function
  | Ok      -> Bytes.make 1 '\x00'
  | Parsing -> Bytes.make 1 '\x01'
  | Cmd     -> Bytes.make 1 '\x02'
(* MD5 digests *)
and forge_digest buffer =
  let s = ref "" in
  Bytes.iter (fun c -> s := Printf.sprintf "%s%02x" !s (int_of_char c)) buffer;
  Digest.from_hex !s
and unforge_digest d =
  let f x1 x2 x3 x4 x5 x6 x7 x8 =
    let a = [| x1 ; x2 ; x3 ; x4 ; x5 ; x6 ; x7 ; x8 |] in
    Bytes.init 8 (fun i -> char_of_int a.(i))
  in
  Scanf.sscanf (Digest.to_hex d) "%2x%2x%2x%2x%2x%2x%2x%2x" f
;;

(* command must be of the form :
  * <code:1> <length:4> <data:length> <digest:16>
  *
  * - code : one of the command codes (in cmd_values)
  * - length : 4 bytes unsigned integer designating the payload length
  * - data : payload
  * - digest : MD5 digest of `data`
  *
  *)
let forge_cmd cmd len data digest =
  if not (is_valid_command_code cmd) then raise (Exn_invalid_command(cmd));
  let computed_len = Bytes.length data in
  if len <> computed_len then raise (Exn_invalid_length(len, computed_len));
  let computed_digest = Digest.bytes data in
  if Digest.compare computed_digest digest <> 0 then
    raise (Exn_invalid_digest(digest, computed_digest));
  match (cmd, len) with
  | (0x01, 4) -> Send(forge_uint32 data)
  | (0x01, _) -> raise (Exn_uncoherent_cmd("Send", data))
  | (0x02, 4) -> Receive(forge_uint32 data)
  | (0x02, _) -> raise (Exn_uncoherent_cmd("Receive", data))
  | (0x03, _) -> Packet(data)
  | (0x04, 1) -> Answer(forge_err data)
  | (0x04, _) -> raise (Exn_uncoherent_cmd("Answer", data))
  | (_,_) -> raise (Exn_uncoherent_cmd(Printf.sprintf "Unknown : cmd:%d len:%d digest:%s" cmd len (Digest.to_hex digest), data))
and unforge_cmd = function
  | Send(l) -> begin
    let data = unforge_uint32 l in
    (Bytes.cat
      (Bytes.of_string "\x01")
      (Bytes.cat
        (unforge_uint32 4)
        (Bytes.cat
          data
          (unforge_digest (Digest.bytes data))
        )
      )
    )
  end
  | Receive(l) -> begin
    let data = unforge_uint32 l in
    (Bytes.cat
      (Bytes.of_string "\x02")
      (Bytes.cat
        (unforge_uint32 4)
        (Bytes.cat
          data
          (unforge_digest (Digest.bytes data))
        )
      )
    )
  end
  | Packet(data) -> begin
    (Bytes.cat
      (Bytes.of_string "\x03")
      (Bytes.cat
        (unforge_uint32 (Bytes.length data))
        (Bytes.cat
          data
          (unforge_digest (Digest.bytes data))
        )
      )
    )
  end
  | Answer(a) -> begin
    let data = unforge_err a in
    (Bytes.cat
      (Bytes.of_string "\x04")
      (Bytes.cat
        (unforge_uint32 1)
        (Bytes.cat
          data
          (unforge_digest (Digest.bytes data))
        )
      )
    )
  end
;;

(* returns a forged command + the unused bytes of the buffer *)
let of_bytes buffer =
  let buf_l = Bytes.length buffer in
  if buf_l < min_size then raise (Exn_read_more(buffer,min_size - buf_l))
  else begin
    let len = forge_uint32 (Bytes.sub buffer 1 4)
    in
    if buf_l - min_size < len then raise (Exn_read_more(buffer, (min_size + len) - buf_l))
    else begin
      let cmd = int_of_char (Bytes.get buffer 0)
      and digest = forge_digest (Bytes.sub buffer (5 + len) 8)
      in (
        forge_cmd cmd len (Bytes.sub buffer 5 len) digest,
        Bytes.sub buffer (min_size + len) (buf_l - (min_size + len))
        )
    end
  end
;;

let to_bytes cmd = unforge_cmd cmd
;;
