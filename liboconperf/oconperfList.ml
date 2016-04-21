open OconperfConfiguration

let average_l l =
  let len = List.length l in
  if len <> 0 then
    Some ((List.fold_left ( +. ) 0.0 l)  /. (float_of_int len))
  else
    None
