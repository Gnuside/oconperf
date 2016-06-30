open Unix

let pidended pid =
  match waitpid [WNOHANG] pid with
  | 0, _            -> false
  | _, WEXITED(_)
  | _, WSIGNALED(_)
  | _, WSTOPPED(_)  -> true

let addr_info addr port =
  getaddrinfo addr (string_of_int port) [AI_SOCKTYPE SOCK_STREAM]
  |> function
      | x :: _ -> x
      | [] -> failwith (Printf.sprintf "Unable to resove %s" addr)
