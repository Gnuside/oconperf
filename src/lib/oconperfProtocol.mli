
val client_run : ?max_time:float -> Unix.file_descr -> float option * float option

val server_run : Unix.file_descr -> unit
