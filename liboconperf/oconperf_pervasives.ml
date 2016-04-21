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
