open Lwt
open Cohttp_lwt_unix
open Middleware

type change_password_form = { old_password : string; new_password : string }

let validate_form form =
  let open Validate in
  let old_password = validate_form form "old_password" in
  let new_password = validate_form form "new_password" in
  Result.bind old_password (fun old_password ->
      Result.bind new_password (fun new_password ->
          Ok { old_password; new_password }))

let respond_error status body = Server.respond_error ~status ~body ()

let change_password req body =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  match session_id with
  | Some session_id -> (
      (*TODO: We are establishing 2 connections in the same block, can we use one? *)
      Redis.redis_conn
      >>= fun conn ->
      Redis.get_session ~conn ~session_id >>= function
      | Some session_data -> (
          match session_data with
          | `Assoc data ->
              let is_authenticated =
                match List.assoc "authenticated" data with
                | `Bool b -> b
                | _ -> false
              in
              let email = List.assoc "email" data |> Yojson.Safe.to_string in
              if is_authenticated then
                Db.with_connection
                  (fun conn ->
                    Model.User.find_user_password_by_email conn email)
                  "DATABASE_URI"
                >>= function
                | Ok password -> (
                    Cohttp_lwt.Body.to_string body >>= fun body ->
                    let form = Uri.query_of_encoded body in
                    match validate_form form with
                    | Ok { old_password; new_password } ->
                        (* TODO: Don't perform operation if old_password = new_password, instead notify the user *)
                        if Option.get password = old_password then
                          Db.with_connection
                            (fun conn ->
                              Model.User.update_user_password conn new_password
                                email)
                            "DATABASE_URI"
                          >>= function
                          | Ok _ ->
                              Server.respond_string ~status:`OK
                                ~body:"Password successfully changed!" ()
                          | Error err ->
                              respond_error `Internal_server_error
                                (Error.to_string (Database_error err))
                        else
                          respond_error `Conflict
                            (Error.to_string Password_mismatch)
                    | Error err ->
                        respond_error `Conflict
                          (Format.sprintf "Error: '%s' in change_password form!"
                             err))
                | Error err ->
                    respond_error `Internal_server_error
                      (Error.to_string (Database_error err))
              else respond_error `Bad_request "Token not authenticated"
          | _ ->
              Server.respond_error ~status:`Bad_request
                ~body:"Invalid session data!" ())
      | None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ())
  | None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
