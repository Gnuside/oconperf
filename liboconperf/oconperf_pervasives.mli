
type byte_power_t =
| Binary_power (* 1024. *)
| Decimal_power (* 1000. *)

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

val show_bytes_human_readable : byte_power_t -> float -> string
