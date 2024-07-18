open Cohttp_lwt_unix
open Lwt

let csrf_protect next req body =
  (*TODO: body seems redundant *)
  let _ = body in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  match List.assoc_opt "session_id" cookies with
  | Some session_id -> (
      match Cohttp.Header.get (Request.headers req) "X-Csrf-token" with
      | Some token -> (
          Lwt_io.printf "CSRF token found in header: %s\n" token >>= fun _ ->
          ();
          Redis.redis_conn >>= fun conn ->
          Redis.validate_csrf_token ~conn ~session_id ~actual_token:token
          >>= function
          | true -> next req body
          | false ->
              Server.respond_error ~status:`Forbidden ~body:"Invalid CSRF token"
                ())
      | None -> (
          Lwt_io.printf "CSRF token not found in header, checking body...\n"
          >>= fun _ ->
          ();
          (* Server.respond_error ~status:`Forbidden ~body:"Missing CSRF token" () *)
          (* If not in header, check form data *)
          Cohttp_lwt.Body.to_string body >>= fun body_string ->
          let form = Uri.query_of_encoded body_string in
          match List.assoc_opt "csrf_token" form with
          | Some [ token ] -> (
              Lwt_io.printf "CSRF token found in body: %s\n" token >>= fun _ ->
              ();
              Redis.redis_conn >>= fun conn ->
              Redis.validate_csrf_token ~conn ~session_id ~actual_token:token
              >>= function
              | true -> next req body
              | false ->
                  Server.respond_error ~status:`Forbidden
                    ~body:"Invalid CSRF token" ())
          | _ ->
              Lwt_io.printf "CSRF token not found in body\n" >>= fun _ ->
              ();
              Server.respond_error ~status:`Forbidden ~body:"Missing CSRF token"
                ()))
  | None -> (
      Redis.redis_conn >>= fun conn ->
      Redis.create_session ~conn ~session_id:Header.session_id
        ~is_authenticated:false ~email:None
      >>= function
      | Ok _ ->
          Server.respond_redirect ~headers:Header.headers
            ~uri:(Uri.of_string "/signup") ()
      | Error err ->
          Server.respond_error ~status:`Internal_server_error ~body:err ())
