open Lwt
open Cohttp_lwt_unix
open Middleware

type signup_form = {
  name : string;
  email : string;
  password : string;
  confirm_password : string;
}
[@@deriving eq]

let validate_form form =
  let open Validate in
  (* TODO: Error handling: when [name] is not the same as [a_name], an internal server error is
     printed, I find that not helpful enough *)
  let name = validate_form form "name" in
  let email = validate_form form "email" in
  let password = validate_form form "password" in
  let confirm_password = validate_form form "confirm_password" in
  Result.bind name (fun name ->
      Result.bind email (fun email ->
          Result.bind password (fun password ->
              Result.bind confirm_password (fun confirm_password ->
                  Ok { name; email; password; confirm_password }))))

let signup body =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let form = Uri.query_of_encoded body in
  match validate_form form with
  | Error err -> Server.respond_error ~status:`Bad_request ~body:err ()
  | Ok { name; email; password; confirm_password } -> (
      match password = confirm_password with
      | true -> (
          (*TODO: We are establishing 3 connections in the same block, can we use one? *)
          Db.with_connection
            (fun conn -> Model.User.find_user_by_email conn email)
            "DATABASE_URI"
          >>= fun res ->
          match res with
          | Ok _ ->
              Server.respond_error ~status:`Conflict
                ~body:(Middleware.Error.to_string Email_used)
                ()
          | _ -> (
              Db.with_connection
                (fun conn -> Model.User.create_user conn name email password)
                "DATABASE_URI"
              >>= function
              | Ok () -> (
                  let body =
                    Form.user_home_page
                    |> Format.asprintf "%a" Tyxml.Html._pp_elt
                  in
                  Server.respond_string ~status:`OK ~body () >>= fun _ ->
                  let session_id =
                    Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string
                  in
                  let csrf_token =
                    Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string
                  in
                  Db.with_connection
                    (fun conn ->
                      Model.User_session.create_user_session conn session_id
                        csrf_token email)
                    "DATABASE_URI"
                  >>= fun res ->
                  match res with
                  | Ok () ->
                      let headers =
                        Cohttp.Header.of_list
                          [
                            ( "Set-Cookie",
                              Format.sprintf "session_id=%s" session_id );
                            ( "Set-Cookie",
                              Format.sprintf "csrf-token=%s" csrf_token );
                          ]
                      in
                      Server.respond_redirect ~headers ~uri:(Uri.of_string "/")
                        ()
                  | Error err ->
                      Server.respond_error ~status:`Internal_server_error
                        ~body:(Error.to_string (Database_error err))
                        ())
              | Error err ->
                  Server.respond_error ~status:`Internal_server_error
                    ~body:(Error.to_string (Database_error err))
                    ()))
      | false ->
          Server.respond_error ~status:`Unauthorized
            ~body:(Error.to_string Password_mismatch)
            ())
