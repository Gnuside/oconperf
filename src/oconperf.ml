open Printf
open OconperfConfiguration

type run_t = Client | Server

let running = ref None
and version = "0.0.1"
and usage = sprintf "\
%s [client|server]\
" Sys.argv.(0)
and addr4 = ref ""
and addr6 = ref ""
and iface_name = ref ""

let show_version () =
  print_endline (sprintf "Version: %s" version);
  exit 0

let args = ref [
  ("--version", Arg.Unit(show_version), "\tShow software version");
  ("-q", Arg.Set(quiet), "\tRun in quiet and parseable mode");
  ("--debug", Arg.Set(debug), "\tRun in debug mode");
  ("-p", Arg.Set_int(port), "\tSet server port (to connect to ; on to listen to)");
  ("-a4", Arg.Set_string(addr4), "\tSet server address (to connect to ; on to listen to)");
  ("-a6", Arg.Set_string(addr6), "\tSet server address (to connect to ; on to listen to)");
]
;;

let server_args = !args
;;

let client_args = !args @ [
  ("-I", Arg.Set_string(iface_name), "\tSet interface name");
  ("-w", Arg.Set_int(max_timeout), "\tSet maximum timeout");
  ("-s", Arg.Set_int(max_size), "\tSet maximum downloaded size");
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

(* Select addr4 if addr6 not specified *)
match !addr4, !addr6 with
 | "", "" -> addr := "127.0.0.1" ; socket_domain := Unix.PF_INET
 | _, ""  -> addr := !addr4 ; socket_domain := Unix.PF_INET
 | "", _  -> addr := !addr6 ; socket_domain := Unix.PF_INET6
 | _, _ -> failwith "Please specify only one IP address."
;;

iface := match !iface_name with
 | ""   -> `Any
 | name -> (`Interface_name name)
;;

let _ =
  exit
  (Unix.handle_unix_error (
    match !running with
     | Some(Server) -> OconperfServer.run
     | Some(Client) -> OconperfClient.run
     | None -> (
         Arg.usage !args usage ;
         failwith "Please specify the action argument."
       )
  ) ())
;;
