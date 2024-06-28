open Lwt
open Cohttp_lwt_unix
open Middleware

type change_password_form = {
  old_password : string;
  new_password : string;
      (* TODO: later we might need confirm_new_password field *)
}

let validate_form form =
  let open Validate in
  let old_password = validate_form form "old_password" in
  let new_password = validate_form form "new_password" in
  Result.bind old_password (fun old_password ->
      Result.bind new_password (fun new_password ->
          Ok { old_password; new_password }))

let change_password req body =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  (* let csrf_token = Cohttp.Header.get (Request.headers req) "csrf_token" in *)
  let csrf_token = List.assoc_opt "csrf-token" cookies in
  match session_id with
  | Some id -> (
      (*TODO: We are establishing 3 connections in the same block, can we use one? *)
      Db.with_connection
        (fun conn -> Model.User_session.get_session conn id)
        "DATABASE_URI"
      >>= fun res ->
      match res with
      | Ok (Some (token, email)) -> (
          if token <> Option.get csrf_token then
            Server.respond_error ~status:`Bad_request ~body:"Invalid csrf token"
              ()
          else
            Db.with_connection
              (fun conn -> Model.User.find_user_password_by_email conn email)
              "DATABASE_URI"
            >>= fun res ->
            match res with
            | Ok password -> (
                Cohttp_lwt.Body.to_string body >>= fun body ->
                let form = Uri.query_of_encoded body in
                match validate_form form with
                | Ok { old_password; new_password } ->
                    (* TODO: Don't perform operation if old_password = new_password, instead notify the user *)
                    if Option.get password = old_password then
                      Db.with_connection
                        (fun conn ->
                          Model.User.update_user conn new_password email)
                        "DATABASE_URI"
                      >>= fun res ->
                      match res with
                      | Ok _ ->
                          Server.respond_string ~status:`OK
                            ~body:"Password successfully changed!" ()
                      | Error err ->
                          Server.respond_error ~status:`Internal_server_error
                            ~body:(Error.to_string (Database_error err))
                            ()
                    else
                      Server.respond_error ~status:`Conflict
                        ~body:(Error.to_string Password_mismatch)
                        ()
                | Error err ->
                    Server.respond_error ~status:`Conflict
                      ~body:
                        (Format.sprintf "Error: '%s' in change_password form!"
                           err)
                      ())
            | Error err ->
                Server.respond_error ~status:`Internal_server_error
                  ~body:(Error.to_string (Database_error err))
                  ())
      | Ok None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
      | Error err ->
          Server.respond_error ~status:`Internal_server_error
            ~body:(Error.to_string (Database_error err))
            ())
  | None -> Server.respond_redirect ~uri:(Uri.of_string "/signup") ()
