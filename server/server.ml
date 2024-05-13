open Lwt
open Cohttp_lwt_unix
open Tyxml

let respond_ok html =
  let body = html |> Format.asprintf "%a" Html._pp_elt in
  Server.respond_string ~status:`OK ~body ()

let respond_not_found =
  Server.respond_string ~status:`OK ~body:"Page not found!" ()

let server =
  let router _conn req _body =
    let path = Uri.path (Request.uri req) in
    match path with
    | "/" -> respond_ok Content.hello
    | "/hello" -> respond_ok Content.to_ocaml
    | "/ocaml.org" -> respond_ok Content.ocaml_world
    | _ -> respond_not_found
  in
  (* Server.make is used to create a server configuration object that can
     be used to create an HTTP instance.

     The server instance is then passed to Server.create to create an actual
     HTTP server instance that listens for incoming requests on a specfied port *)
  let server =
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:router ())
  in
  server >>= fun _ -> Lwt_io.printf "Server listening on port 8000\n"
(*FIXME: this is not printed on terminal when the server starts *)

let () = Lwt_main.run server
