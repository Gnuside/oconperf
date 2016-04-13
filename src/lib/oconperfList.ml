open OconperfConfiguration

let average_l l =
  (List.fold_left ( +. ) 0.0 l)  /. (float_of_int (List.length l))
