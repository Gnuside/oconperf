open Oconperf_configuration

let print_message message =
  if !quiet == false
  then print_endline message
  else ()

let print_debug message =
  if !debug == true
  then print_endline message
  else ()

let print_error message =
  prerr_endline message

let print_message_f message_f =
  if !quiet == false
  then print_endline (message_f ())
  else ()

let print_debug_f message_f =
  if !debug == true
  then print_endline (message_f ())
  else ()

type byte_power_t =
| Binary_power (* 1024. *)
| Decimal_power (* 1000. *)

let bytes_unit_to_string = function
| 0 -> ""
| 1 -> "K"
| 2 -> "M"
| 3 -> "G"
| 4 -> "T"
| 5 -> "P"
| 6 -> "E"
| 7 -> "Z"
| _ -> "Y"
and byte_power = function
| Binary_power -> 1024.
| Decimal_power -> 1000.

let show_bytes_human_readable a = function
| 0. -> "0 iB"
| v -> begin
  let a' = byte_power a in
  let i = min (max (floor ((log v) /. (log a'))) 0.) 8. in
  Printf.sprintf "%.2f %siB / s" (v /. (a' ** i)) (bytes_unit_to_string (int_of_float i))
end
