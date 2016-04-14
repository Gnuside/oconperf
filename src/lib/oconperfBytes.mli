
(* Convert byte reprentation to hex representation *)
val bytes_to_hex : Bytes.t -> bool -> Bytes.t

val random_fill : Bytes.t -> Bytes.t -> int -> unit

val create_random_bytes : int -> Bytes.t
