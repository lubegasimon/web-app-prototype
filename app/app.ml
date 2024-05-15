open Lwt
open Cohttp_lwt_unix
open Tyxml

type user = { name : string; id : string } [@@deriving yojson]

let respond_ok html =
  let body = html |> Format.asprintf "%a" Html._pp_elt in
  Server.respond_string ~status:`OK ~body ()

let split_path path =
  let segments = String.split_on_char '/' path in
  List.filter (fun seg -> seg <> "") segments

let server =
  let router _conn req _body =
    let uri = Request.uri req in
    let path = Uri.path uri in
    match split_path path with
    | [] -> respond_ok Content.hello
    | [ "hello" ] -> respond_ok Content.to_ocaml
    | [ "ocaml.org" ] -> respond_ok Content.ocaml_world
    | [ "user"; id ] ->
        let admin = { name = "simon"; id } in
        let yojson_of_admin = user_to_yojson admin in
        let str_of_yojson = Yojson.Safe.to_string yojson_of_admin in
        Server.respond_string ~status:`OK ~body:str_of_yojson ()
    | _ -> Server.respond_not_found ()
  in
  let server =
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:router ())
  in
  server >>= fun _ -> Lwt_io.printf "Server listening on port 8000\n"
(*FIXME: this is not printed on terminal when the server starts *)
