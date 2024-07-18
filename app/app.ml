open Lwt
open Cohttp_lwt_unix
open Middleware

let sanitize_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

let callback _conn req body =
  let open Handler in
  let uri = Request.uri req in
  let meth = Request.meth req in
  let path = Uri.path uri in
  match (meth, sanitize_path path) with
  | `GET, [] ->
      (* TODO: protect visitor requests against csrf attacks *)
      Root.root req body
  | `GET, [ "signup" ] -> (
      (* let body = Form.signup |> Format.asprintf "%a" Tyxml.Html._pp_elt in
         Server.respond_string ~status:`OK ~body () *)
      let respond_ok html headers =
        let body = html |> Format.asprintf "%a" Tyxml.Html._pp_elt in
        Server.respond_string ~headers ~status:`OK ~body ()
      in
      let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
      let session_id =
        match List.assoc_opt "session_id" cookies with
        | Some id -> id
        | None -> ""
        (* TODO: Not sure if we need to create one *)
      in
      Redis.redis_conn >>= fun conn ->
      Redis.get_session ~conn ~session_id >>= function
      | Some session_data -> (
          match session_data with
          | `Assoc data ->
              let csrf_token =
                List.assoc "csrf_token" data |> Yojson.Safe.to_string
              in
              let body =
                Form.signup csrf_token
                |> Format.asprintf "%a" Tyxml.Html._pp_elt
              in
              Server.respond_string ~status:`OK ~body ()
          | _ ->
              Server.respond_error ~status:`Unauthorized
                ~body:"Invalid session data!" ())
      | _ -> (
          Redis.create_session ~conn ~session_id:Header.session_id
            ~is_authenticated:false ~email:None
          >>= function
          | Ok _ -> respond_ok Form.visitor_home_page Header.headers
          | Error err ->
              Server.respond_error ~status:`Internal_server_error ~body:err ()))
  | `POST, [ "signup" ] ->
      (* TODO: Understand why the following causes a very serious bug:
         -  Csrf_protect.csrf_protect Signup.signup req body.contents
         (Because it seems not to make sense to convert to_string then back)
      *)
      Cohttp_lwt.Body.to_string body >>= fun body ->
      Csrf_protect.csrf_protect Signup.signup req
        (Cohttp_lwt.Body.of_string body)
  | `POST, [ "logout" ] ->
      (* TODO: protect logout operation against csrf attacks *)
      Logout.logout req body
  | `POST, [ "change_password" ] ->
      (* TODO: protect change_password operation against csrf attacks *)
      Password.change_password req body
  | `GET, [ "get-csrf-token" ] ->
      let headers =
        Cohttp.Header.of_list [ ("X-Csrf-token", Header.csrf_token) ]
      in
      Server.respond_string ~headers ~status:`OK ~body:"" ()
  | _ ->
      Printf.printf "No matching route found\n";
      Server.respond_not_found ()

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
