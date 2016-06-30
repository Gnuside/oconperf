open Printf
open Oconperf_configuration

type run_t = Client | Server

let running = ref None
and version = "0.0.1"
and usage = sprintf "\
%s [client|server]\
" Sys.argv.(0)
and iface_name = ref ""

let show_version () =
  print_endline (sprintf "Version: %s" version);
  exit 0

let args = ref (Arg.align [
  ("--version", Arg.Unit(show_version), " Show software version");
  ("-q", Arg.Set(quiet), " Run in quiet and parseable mode");
  ("--debug", Arg.Set(debug), " Run in debug mode");
  ("-p", Arg.Set_int(port), " Set server port (to connect to ; on to listen to)");
  ("-a", Arg.Set_string(addr), " Set server address (to connect to ; on to listen to)");
  ("-S", Arg.Set_int(max_packet_size), " Set maximum packet size");
])

let server_args = !args

and client_args = !args @ [
  ("-I", Arg.Set_string(iface_name), " Set interface name");
  ("-w", Arg.Set_int(max_timeout), " Set maximum timeout");
  ("-s", Arg.Set_int(max_size), " Set maximum downloaded size");
  ("-u", Arg.Set(test_upload), " Test upload speed instead of download");
  ("-h", Arg.Set(human_readable), " Print sizes in powers of 1024 (e.g., 1023M)");
]

let anon_fun arg = match !Arg.current with
 | 1 -> begin
  match arg with
   | "server" -> begin
    running := Some(Server) ;
    args := Arg.align server_args
   end
   | "client" -> begin
    running := Some(Client) ;
    args := Arg.align client_args
   end
   | _ -> failwith (sprintf "Unknown action '%s'" arg)
  end
 | _  -> failwith (sprintf "Argument '%s' ignored." arg)

let parse_arguments () =
  Arg.parse_dynamic args (anon_fun) usage ;
  iface := match !iface_name with
   | ""   -> `Any
   | name -> (`Interface_name name)

let _ =
  parse_arguments () ;
  exit (Unix.handle_unix_error (fun () -> begin
    match !running with
     | Some(Server) -> Oconperf_server.run ~max_packet_size: !max_packet_size
                                           ~max_pending_request: !mpc
                                           !addr
                                           !port
     | Some(Client) -> Oconperf_client.run ~test_upload: !test_upload
                                           ~human_readable: !human_readable
                                           ~max_time: (float_of_int !max_timeout)
                                           ~max_size: !max_size
                                           ~max_packet_size: !max_packet_size
                                           ~iface: !iface
                                           !addr
                                           !port
     | None -> (
         Arg.usage !args usage ;
         failwith "Please specify the action argument."
       )
  end) ())
