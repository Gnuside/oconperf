
val client_run : ?test_upload:bool -> ?max_time:float -> ?max_size:int -> ?max_packet_size:int -> Unix.file_descr -> float option * float option

val server_run : ?max_packet_size:int -> Unix.file_descr -> unit
