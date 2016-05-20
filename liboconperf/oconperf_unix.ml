open Unix

let pidended pid =
  match waitpid [WNOHANG] pid with
  | 0, _            -> false
  | _, WEXITED(_)   -> true
  | _, WSIGNALED(_) -> true
  | _, WSTOPPED(_)  -> true

