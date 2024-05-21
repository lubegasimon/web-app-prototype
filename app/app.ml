open Lwt
open Cohttp_lwt_unix
open Tyxml
open Datastore

let respond_string body = Server.respond_string ~status:`OK ~body ()

let respond_ok html =
  let body = html |> Format.asprintf "%a" Html._pp_elt in
  respond_string body

let split_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

let ( let* ) = Lwt.bind

let handle_form_post body =
  let* body_str = Cohttp_lwt.Body.to_string body in
  let form_data = Uri.query_of_encoded body_str in
  let id = List.assoc "id" form_data |> List.hd in
  (* TODO: Error handling: when [name] is not the same as [a_name], an internal server error is
     printed, I find that not helpful enough *)
  let name = List.assoc "name" form_data |> List.hd in
  Datastore.create_user id name;
  let user = Hashtbl.find Datastore.hashtbl id in
  let response_body =
    Format.sprintf "User %s with ID %s created successfully!" user id
  in
  respond_string response_body

let server =
  let router _conn req body =
    let uri = Request.uri req in
    let meth = Request.meth req in
    let path = Uri.path uri in
    match (meth, split_path path) with
    | `GET, [] -> respond_ok Content.home
    | `GET, [ "signup" ] -> respond_ok Content.signup
    | `GET, [ "create_acc" ] -> respond_ok Content.create_user
    | `GET, [ "users_len" ] ->
        respond_string (Hashtbl.length Datastore.hashtbl |> string_of_int)
    | `POST, [ "add_user" ] -> handle_form_post body
    | _ -> Server.respond_not_found ()
  in

  let server =
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:router ())
  in
  server >>= fun _ -> Lwt_io.printf "Server listening on port 8000\n"
(*FIXME: this is not printed on terminal when the server starts *)
