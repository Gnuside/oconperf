open Printf

type run_t = Client | Server;;

let running = ref Client
and version = "0.0.1";;


let usage = sprintf "\
%s [-s]\
" Sys.argv.(0)
and show_version () =
  print_endline (sprintf "Version: %s" version);
  exit 0
;;


Arg.parse [
  ("--version", Arg.Unit(show_version), "Show software version");
  ("-s", Arg.Unit(fun () -> running := Server), "Run the server part");
]
(fun a -> failwith (sprintf "%s: anonymous argument not supported" a)) usage;

exit
(
  match !running with
  | Server -> OconperfServer.run ()
  | Client -> OconperfClient.run ()
)
