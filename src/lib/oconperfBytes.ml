open Printf

let bytes_to_hex b =
  let buffer = Bytes.create ((Bytes.length b) * 3) in
  let insert i c =
    let short_b = Bytes.of_string (sprintf "%02X " (int_of_char c)) in
    Bytes.blit short_b 0 buffer (i*3) 3
  in
  Bytes.iteri insert b;
  buffer

(* fill a bytes buffer with random data *)
let random_fill bytes random_buffer random_buffer_size =
  let len = Bytes.length bytes in
  let i = ref 0
  and l = ref (min len (Random.int random_buffer_size)) in
  while !i < len do
    let i_r = Random.int !l in
    let len_r = min (len - !i) (random_buffer_size - i_r) in
    Bytes.blit random_buffer i_r bytes !i len_r;
    i := !i + len_r;
    l := min (len - !i) (Random.int random_buffer_size)
  done

let create_random_bytes size =
  Bytes.init size (fun _ -> char_of_int (Random.int 256))
