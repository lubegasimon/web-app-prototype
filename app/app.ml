open Lwt
open Cohttp_lwt_unix

let sanitize_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

let callback _conn req body =
  let open Handler in
  let uri = Request.uri req in
  let meth = Request.meth req in
  let path = Uri.path uri in
  match (meth, sanitize_path path) with
  | `GET, [] -> Root.root req
  | `GET, [ "signup" ] ->
      let body = Form.signup |> Format.asprintf "%a" Tyxml.Html._pp_elt in
      Server.respond_string ~status:`OK ~body ()
  | `POST, [ "signup" ] -> Signup.signup body
  | `POST, [ "logout" ] -> Logout.logout req
  | `POST, [ "change_password" ] -> Password.change_password req body
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
