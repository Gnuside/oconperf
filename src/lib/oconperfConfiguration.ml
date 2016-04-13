
let quiet = ref false
and port = ref 4217
and socket_domain = ref Unix.PF_INET
and addr = ref ""
and mpc = ref 10 (* max pending clients *)
and iface = ref ""
and max_timeout = ref 0
