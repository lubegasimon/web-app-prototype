open Cohttp_lwt_unix
open Cohttp
open Lwt.Infix

let send_request meth path body =
  let uri = Uri.of_string ("http://localhost:8001" ^ path) in
  let body = match body with None -> `String "" | Some body -> `String body in
  Client.call meth uri ~body >>= fun (res, body) ->
  let code = Code.code_of_status @@ Response.status @@ res in
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return (code, body)

let test_get_home _ () =
  let expected_code = 200 in
  let expected_body =
    "<html><head><title>Expense tracker</title></head><body><div>\n\
    \        Hello, Welcome to expense tracker!\n\
    \        <br/>\n\
    \        <div><a href=\"signup\"> Sign up </a> </div>\n\
    \      </div></body></html>"
  in
  send_request `GET "/" None >>= fun (actual_code, actual_body) ->
  Alcotest.(check int) "same status code" expected_code actual_code;
  Alcotest.(check string) "same body" expected_body actual_body;
  Lwt.return ()

let test_get_signup _ () =
  let expected_code = 200 in
  send_request `GET "/signup" None >>= fun (actual_code, _) ->
  Alcotest.(check int) "same status code" expected_code actual_code;
  Lwt.return ()

let signup_handler body =
  let open Handler in
  let form = Uri.query_of_encoded body in
  let { Signup.name; email; password; confirm_password } =
    match Signup.validate_form form with
    | Ok fields -> fields
    | Error err -> raise (failwith err)
  in
  (match password = confirm_password with
  | true -> (
      Db.with_connection
        (fun conn -> Model.User.create_user conn name email password)
        "E2E_TEST_DATABASE_URI"
      >>= function
      | Ok () ->
          Lwt.return @@ Format.sprintf "User %s created successfully!" name
      | Error err -> Lwt.return @@ Format.sprintf "%s\n" (Caqti_error.show err))
  | false -> Lwt.return @@ Format.sprintf "Passwords don't match!\n")
  >>= fun response -> Lwt.return @@ Format.sprintf "%s\n" response

let body =
  let body =
    Uri.encoded_of_query
      [
        ("name", [ "John Doe" ]);
        ("email", [ "johndoe@example.com" ]);
        ("password", [ "johndoe" ]);
        ("confirm_password", [ "johndoe" ]);
      ]
  in
  signup_handler body >>= fun body -> Lwt.return body

let test_post_create_user _ () =
  let expected_code = 200 in
  let expected_body = "User John Doe created successfully!\n" in
  body >>= fun body ->
  send_request `POST "/signup" (Some body) >>= fun (actual_code, actual_body) ->
  Alcotest.(check int) "same status code" expected_code actual_code;
  Alcotest.(check string) "same body" expected_body actual_body;
  Lwt.return ()

let callback _conn req _body =
  let uri = Request.uri req in
  let meth = Request.meth req in
  let path = Uri.path uri in
  match (meth, App.sanitize_path path) with
  | `GET, [] -> Handler.Root.root req
  | `GET, [ "signup" ] ->
      let body = Form.signup |> Format.asprintf "%a" Tyxml.Html._pp_elt in
      Server.respond_string ~status:`OK ~body ()
  | `POST, [ "signup" ] ->
      body >>= fun body -> Server.respond_string ~status:`OK ~body ()
  | _ -> Server.respond_not_found ()

(* A mock server *)
let _ =
  Middleware.Log.log;
  Server.create ~mode:(`TCP (`Port 8001)) (Server.make ~callback ())

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "e2e test"
       [
         ("test_get_home", [ test_case "test_get_home" `Quick test_get_home ]);
         ( "test_get_signup",
           [ test_case "test_get_signup" `Quick test_get_signup ] );
         ( "test_post_create_user",
           [ test_case "test_post_create_user" `Quick test_post_create_user ] );
       ]
