open Printf

let bytes_to_hex b with_space =
  let pattern = format_of_string (if with_space then "%02X " else "%02X")
  and out_char_width = if with_space then 3 else 2 in
  let buffer = Bytes.create ((Bytes.length b) * out_char_width) in
  let insert i c =
    let short_b = Bytes.of_string (sprintf pattern (int_of_char c)) in
    Bytes.blit short_b 0 buffer (i*out_char_width) out_char_width
  in
  Bytes.iteri insert b;
  buffer

(* fill a bytes buffer with random data *)
let random_fill bytes len random_buffer random_buffer_size =
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
