
type error_t =
  | Ok
  | Parsing
  | Cmd
  | Read_failed

type cmd_t =
  | Send of int (* command asking to the server to send data *)
  | Receive of int (* command asking to the server to receive data *)
  | Packet of bytes (* Command containing data *)
  | Answer of error_t

val min_size : int

exception Exn_invalid_command of int
exception Exn_invalid_error_code of char
exception Exn_invalid_length of int * int
exception Exn_invalid_digest of Digest.t * Digest.t
exception Exn_uncoherent_cmd of string * bytes
exception Exn_read_more of bytes * int (* (buf,r) : read r more bytes for buf to be complete *)

val err_to_string : error_t -> String.t

val cmd_to_string : cmd_t -> String.t

(* returns a forged command + the unused bytes of the buffer *)
val of_bytes : Bytes.t -> int -> cmd_t * Bytes.t

val to_bytes : cmd_t -> Bytes.t
