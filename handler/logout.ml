open Lwt
open Cohttp_lwt_unix

let logout req =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  match session_id with
  | Some id -> (
      Db.with_connection
        (fun conn -> Model.User_session.close_session conn id)
        "DATABASE_URI"
      >>= fun res ->
      match res with
      | Ok _ -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
      | Error err ->
          Server.respond_error ~status:`Internal_server_error
            ~body:(Middleware.Error.to_string (Database_error err))
            ())
  | None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
