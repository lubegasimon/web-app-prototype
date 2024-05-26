open Lwt

let uri =
  match Sys.getenv_opt "DATABASE_URI" with
  | Some uri -> Uri.of_string uri
  | None -> Uri.of_string "Postgresql://"

(* establishes a database connection*)
let connect = Caqti_lwt.connect uri

(* passes a database connection to a client [f] *)
let with_connection f =
  connect >>= function Ok conn -> f conn | Error err -> Lwt.return (Error err)
