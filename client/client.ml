open Lwt
open Cohttp
open Cohttp_lwt_unix

let home_page =
  Client.get (Uri.of_string "http://localhost:8000") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  (* we use Lwt_main.run to run the event loop, and return the
     final value of the body. *)
  let body = Lwt_main.run home_page in
  print_endline body
