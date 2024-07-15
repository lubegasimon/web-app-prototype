open Cohttp_lwt_unix
open Middleware
open Lwt

let respond_ok html headers =
  let body = html |> Format.asprintf "%a" Tyxml.Html._pp_elt in
  Server.respond_string ~headers ~status:`OK ~body ()

let root req body =
  (* TODO: body seems redundant *)
  let _ = body in
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id =
    match List.assoc_opt "session_id" cookies with Some id -> id | None -> ""
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
          let is_authenticated =
            match List.assoc "authenticated" data with
            | `Bool b -> b
            | _ -> false
          in
          let headers =
            Cohttp.Header.of_list
              [ ("X-Csrf-token", Format.sprintf "%s" csrf_token) ]
          in
          if is_authenticated then respond_ok Form.user_home_page headers
          else respond_ok Form.visitor_home_page headers
      | _ ->
          Server.respond_error ~status:`Bad_request
            ~body:"Invalid session data!" ())
  | _ -> (
      Redis.create_session ~conn ~session_id:Header.session_id
        ~is_authenticated:false ~email:None
      >>= function
      | Ok _ -> respond_ok Form.visitor_home_page Header.headers
      | Error err ->
          Server.respond_error ~status:`Internal_server_error ~body:err ())
