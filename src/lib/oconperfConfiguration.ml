
let quiet = ref false
and debug = ref false
and human_readable = ref false
and port = ref 4217
and socket_domain = ref Unix.PF_INET
and addr = ref ""
and mpc = ref 10 (* max pending clients *)
and iface : [ `Any | `Interface_name of string ] ref = ref `Any
and max_timeout = ref 2
and max_size = ref 0
and max_packet_size = ref 10485760 (* 10 Mio *)
and test_upload = ref false
