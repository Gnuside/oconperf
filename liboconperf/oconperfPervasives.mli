
(* Print a message to STDOUT unless quiet flag is enabled *)
val print_message : string -> unit

(* Print a message to STDOUT if debug flag is enabled *)
val print_debug : string -> unit

(* Print a message to STDERR *)
val print_error : string -> unit

(* Print a message to STDOUT unless quiet flag is enabled *)
val print_message_f : (unit -> string) -> unit

(* Print a message to STDOUT if debug flag is enabled *)
val print_debug_f : (unit -> string) -> unit
