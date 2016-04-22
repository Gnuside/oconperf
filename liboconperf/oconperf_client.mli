
val speed_test : ?test_upload:bool -> ?max_time:float -> ?max_size:int -> ?max_packet_size:int -> ?iface:[ `Any | `Interface_name of string ] -> string -> int -> (float option * float option)

val run : ?test_upload:bool -> ?human_readable:bool -> ?max_time:float -> ?max_size:int -> ?max_packet_size:int -> ?iface:[ `Any | `Interface_name of string ] -> string -> int -> int
