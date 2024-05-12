open Lwt
open Cohttp
open Cohttp_lwt_unix

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    ( body |> Cohttp_lwt.Body.to_string >|= fun _ ->
      Printf.sprintf "Uri: %s\nMethod: %s\nHeaders: %s\nBody: %s\n" uri meth
        headers "Hello, World!" )
    >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  (* Server.make is used to create a server configuration object that can
     be used to create an HTTP instance.

     The server instance is then passed to Server.create to create an actual
     HTTP server instance that listens for incoming requests on a specfied port *)
  let server =
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())
  in
  server >>= fun _ -> Lwt_io.printf "Server listening on port 8000\n"
(*FIXME: this is not printed on terminal when the server starts *)

let () = Lwt_main.run server
