
type error_t =
  | Ok
  | Parsing
  | Cmd
  | Read_failed
  | Read_big

type header_t = int * int (* command and data size *)

type cmd_t =
  | Send of int (* command asking to the server to send data *)
  | Receive of int (* command asking to the server to receive data *)
  | Packet of int (* Command containing data *)
  | Answer of error_t
  | Bye

val min_size : int

exception Exn_invalid_command of int
exception Exn_invalid_error_code of char
exception Exn_invalid_length of int * int
exception Exn_invalid_digest of Digest.t * Digest.t
exception Exn_uncoherent_cmd of string * bytes
exception Exn_read_more of bytes * int (* (buf,r) : read r more bytes for buf to be complete *)

val err_to_string : error_t -> String.t

val cmd_to_string : cmd_t -> String.t

(*
 * Returns the header cmd * data_size. This method assume that you have a buffer
 * in the correct size (Oconperf_protocol_base.min_size)
 *)
val of_bytes_header : Bytes.t -> int -> int -> header_t option
val of_bytes_body : Bytes.t -> int -> int -> header_t -> cmd_t option

(* returns a forged Some(command) + the size of remaining data or None + missing packet length *)
val of_bytes : Bytes.t -> int -> int -> (cmd_t option * int)

val to_bytes : cmd_t -> Bytes.t
