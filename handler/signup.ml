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

let respond_error status body = Server.respond_error ~status ~body ()

let signup body =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let form = Uri.query_of_encoded body in
  match validate_form form with
  | Error err -> respond_error `Bad_request err
  | Ok { name; email; password; confirm_password } -> (
      match password = confirm_password with
      | true -> (
          (*TODO: We are establishing 3 connections in the same block, can we use one? *)
          Db.with_connection
            (fun conn -> Model.User.find_user_by_email conn email)
            "DATABASE_URI"
          >>= fun res ->
          match res with
          | Ok _ -> respond_error `Conflict (Error.to_string Email_used)
          | _ -> (
              Db.with_connection
                (fun conn -> Model.User.create_user conn name email password)
                "DATABASE_URI"
              >>= function
              | Ok () -> (
                  let open Header in
                  let body =
                    Form.user_home_page
                    |> Format.asprintf "%a" Tyxml.Html._pp_elt
                  in
                  Server.respond_string ~status:`OK ~body () >>= fun _ ->
                  Db.with_connection
                    (fun conn ->
                      Model.User_session.create_user_session conn session_id
                        csrf_token email)
                    "DATABASE_URI"
                  >>= fun res ->
                  match res with
                  | Ok () ->
                      Server.respond_redirect ~headers ~uri:(Uri.of_string "/")
                        ()
                  | Error err ->
                      respond_error `Internal_server_error
                        (Error.to_string (Database_error err)))
              | Error err ->
                  respond_error `Internal_server_error
                    (Error.to_string (Database_error err))))
      | false -> respond_error `Unauthorized (Error.to_string Password_mismatch)
      )
