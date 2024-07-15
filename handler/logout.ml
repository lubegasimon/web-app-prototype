open Lwt
open Cohttp_lwt_unix
open Middleware

let logout req body =
  (* TODO: body seems redundant *)
  let _ = body in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  match session_id with
  | Some session_id -> (
      Redis.redis_conn >>= fun conn ->
      Redis.delete_session ~conn ~session_id >>= function
      | Ok _ -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
      | Error err ->
          Server.respond_error ~status:`Internal_server_error
            ~body:(Error.to_string (Database_error err))
            ())
  | None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
