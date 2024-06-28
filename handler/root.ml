open Cohttp_lwt_unix

let respond_ok html =
  let body = html |> Format.asprintf "%a" Tyxml.Html._pp_elt in
  Server.respond_string ~status:`OK ~body ()

let root req =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract (Request.headers req) in
  let session_id = List.assoc_opt "session_id" cookies in
  match session_id with
  | Some _ -> respond_ok Form.user_home_page
  | None -> respond_ok Form.visitor_home_page
