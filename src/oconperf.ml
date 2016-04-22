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

let args = ref [
  ("--version", Arg.Unit(show_version), "\tShow software version");
  ("-q", Arg.Set(quiet), "\tRun in quiet and parseable mode");
  ("--debug", Arg.Set(debug), "\tRun in debug mode");
  ("-p", Arg.Set_int(port), "\tSet server port (to connect to ; on to listen to)");
  ("-a", Arg.Set_string(addr), "\tSet server address (to connect to ; on to listen to)");
  ("-S", Arg.Set_int(max_packet_size), "\tSet maximum packet size");
]
;;

let server_args = !args
;;

let client_args = !args @ [
  ("-I", Arg.Set_string(iface_name), "\tSet interface name");
  ("-w", Arg.Set_int(max_timeout), "\tSet maximum timeout");
  ("-s", Arg.Set_int(max_size), "\tSet maximum downloaded size");
  ("-u", Arg.Set(test_upload), "\tTest upload speed instead of download");
  ("-h", Arg.Set(human_readable), "\tPrint sizes in powers of 1024 (e.g., 1023M)");
]
;;

let anon_fun arg = match !Arg.current with
 | 1 -> begin
  match arg with
   | "server" -> running := Some(Server) ; args := server_args
   | "client" -> running := Some(Client) ; args := client_args
   | _ -> failwith (sprintf "Unknown action '%s'" arg)
  end
 | _  -> failwith (sprintf "Argument '%s' ignored." arg)
;;

Arg.parse_dynamic args anon_fun usage;;

iface := match !iface_name with
 | ""   -> `Any
 | name -> (`Interface_name name)
;;

let _ =
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
;;
