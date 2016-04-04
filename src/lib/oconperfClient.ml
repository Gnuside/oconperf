open OconperfConfiguration
open OconperfProtocol
open Printf
open Unix


let run () =
  let buf_size = 512*1024 in
  let buffer = Bytes.create buf_size
  and recv_i = ref 0
  and connected = ref true in
  print_endline (sprintf "Client connects to %s:%d" !addr4 !port);
  let s4 = socket PF_INET SOCK_STREAM 0
  and sa = ADDR_INET(inet_addr_of_string !addr4, !port)
  in
  connect s4 sa;
  while !connected do
    recv_i := recv s4 buffer 0 buf_size [];
    if !recv_i = 0 then
      connected := false
    else
      print_endline (sprintf "Recv: '%s'" (Bytes.sub_string buffer 0 !recv_i))
  done;
  print_endline "Client disconnected.";
  0
