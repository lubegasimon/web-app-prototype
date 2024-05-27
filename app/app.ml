open Lwt
open Cohttp_lwt_unix
open Tyxml

type signup_form = {
  name : string;
  email : string;
  password : string;
  confirm_password : string;
}
[@@deriving eq]

let respond_string body = Server.respond_string ~status:`OK ~body ()

let respond_ok html =
  let body = html |> Format.asprintf "%a" Html._pp_elt in
  respond_string body

let split_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

let field_values form_data =
  let field_value field_name = List.assoc field_name form_data |> List.hd in
  (* TODO: Error handling: when [name] is not the same as [a_name], an internal server error is
     printed, I find that not helpful enough *)
  let name = field_value "name" in
  let email = field_value "email" in
  let password = field_value "password" in
  let confirm_password = field_value "confirm_password" in
  { name; email; password; confirm_password }

let form_handler body =
  let ( let* ) = Lwt.bind in
  let return = Lwt.return in
  let* body_str = Cohttp_lwt.Body.to_string body in
  let form_data = Uri.query_of_encoded body_str in
  let { name; email; password; confirm_password } = field_values form_data in
  let* response =
    match password = confirm_password with
    | true -> (
        Db.with_connection (fun conn ->
            Model.User.create_user conn name email password)
        >>= function
        | Ok () ->
            Format.sprintf "User %s created successfully!\n" name |> return
        | Error err -> Format.sprintf "%s\n" (Caqti_error.show err) |> return)
    | false -> Format.sprintf "Passwords don't match!\n" |> return
  in
  let response_body = Format.sprintf "%s\n" response in
  respond_string response_body

let server =
  let router _conn req body =
    let uri = Request.uri req in
    let meth = Request.meth req in
    let path = Uri.path uri in
    match (meth, split_path path) with
    | `GET, [] -> respond_ok Form.home
    | `GET, [ "signup" ] -> respond_ok Form.signup
    | `POST, [ "create_user" ] -> form_handler body
    | _ -> Server.respond_not_found ()
  in

  let server =
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:router ())
  in
  server >>= fun _ -> Lwt_io.printf "Server listening on port 8000\n"
(*FIXME: this is not printed on terminal when the server starts *)
