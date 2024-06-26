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

let split_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

(* TODO: write unit test *)
let field_required field value =
  match value <> "" with
  | true -> Ok true
  | false -> Error (Format.sprintf "Field %s is required!\n" field)

let field_values form_data =
  let field_value field =
    match List.assoc_opt field form_data with
    | Some x -> List.hd x |> String.trim
    | None -> failwith (Format.sprintf "Field %s is empty!" field)
  in
  let validate field =
    let value = field_value field in
    match field_required field value with
    | Ok _ -> Ok value
    | Error e -> Error e
  in
  (* TODO: Error handling: when [name] is not the same as [a_name], an internal server error is
     printed, I find that not helpful enough *)
  let name = validate "name" in
  let email = validate "email" in
  let password = validate "password" in
  let confirm_password = validate "confirm_password" in
  Result.bind name (fun name ->
      Result.bind email (fun email ->
          Result.bind password (fun password ->
              Result.bind confirm_password (fun confirm_password ->
                  Ok { name; email; password; confirm_password }))))

let form_handler body =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let form_data = Uri.query_of_encoded body in
  match field_values form_data with
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
                ~body:"Email already used!\n" ()
          | _ -> (
              Db.with_connection
                (fun conn -> Model.User.create_user conn name email password)
                "DATABASE_URI"
              >>= function
              | Ok () -> (
                  let body =
                    Form.user_home |> Format.asprintf "%a" Html._pp_elt
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
                            ("X-Csrf-Token", Format.sprintf "%s" csrf_token);
                          ]
                      in
                      Server.respond_redirect ~headers ~uri:(Uri.of_string "/")
                        ()
                  | Error err ->
                      Server.respond_error ~status:`Internal_server_error
                        ~body:
                          (Format.sprintf "Database error: %s\n"
                             (Caqti_error.show err))
                        ())
              | Error err ->
                  Server.respond_error ~status:`Internal_server_error
                    ~body:
                      (Format.sprintf "Database error: %s\n"
                         (Caqti_error.show err))
                    ()))
      | false ->
          Server.respond_error ~status:`Unauthorized
            ~body:"Passwords don't match!\n" ())

let respond_ok html =
  let body = html |> Format.asprintf "%a" Html._pp_elt in
  Server.respond_string ~status:`OK ~body ()

let root_handler req =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  match session_id with
  | Some _ -> respond_ok Form.user_home
  | None -> respond_ok Form.site_home

let callback _conn req body =
  let uri = Request.uri req in
  let meth = Request.meth req in
  let path = Uri.path uri in
  match (meth, split_path path) with
  | `GET, [] -> root_handler req
  | `GET, [ "signup" ] -> respond_ok Form.signup
  | `POST, [ "signup" ] -> form_handler body
  | _ -> Server.respond_not_found ()

let server =
  Lwt.catch
    (fun () ->
      (*FIXME: this is not printed on terminal when the server starts *)
      let server =
        Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
      in
      server >>= fun _ ->
      Format.printf "Server listening on port 8000\n" |> Lwt.return)
    (function
      | Unix.Unix_error (err, func, arg) ->
          Lwt_io.eprintf "Error starting server: %s in %s(%s)"
            (Unix.error_message err) func arg
      | exn -> Lwt_io.eprintlf "Unexpected error: %s" (Printexc.to_string exn))
