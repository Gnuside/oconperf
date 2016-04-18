open OconperfConfiguration

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
