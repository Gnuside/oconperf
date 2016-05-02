open Unix

let pidended pid =
  match waitpid [WNOHANG] pid with
  | 0, _ -> false
  | _, WEXITED(_)
  | _, WSIGNALED(_)
  | _, WSTOPPED(_) -> true
;;
