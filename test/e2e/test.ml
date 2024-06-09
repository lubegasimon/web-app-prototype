open Cohttp_lwt_unix
open Cohttp
open Lwt.Infix
open Caqti_type
open Caqti_request.Infix

let send_request meth path body =
  let uri = Uri.of_string ("http://localhost:8001" ^ path) in
  let body = match body with None -> `String "" | Some body -> `String body in
  Client.call meth uri ~body >>= fun (res, body) ->
  let code = Code.code_of_status @@ Response.status @@ res in
  Cohttp_lwt.Body.to_string body >>= fun body -> Lwt.return (code, body)

let test_get_home _ () =
  let expected_code = 200 in
  let expected_body =
    "<div>\n\
    \  Hello, Welcome to expense tracker!\n\
    \  <br/>\n\
    \  <a href=\"signup\"> Sign up </a>\n\
     </div>"
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

let db_uri = "TEST_DATABASE_URI"

let form_handler body =
  let ( let* ) = Lwt.bind in
  let return = Lwt.return in
  let form_data = Uri.query_of_encoded body in
  let { App.name; email; password; confirm_password } =
    App.field_values form_data
  in
  let* response =
    match password = confirm_password with
    | true -> (
        Db.with_connection
          (fun conn -> Model.User.create_user conn name email password)
          db_uri
        >>= function
        | Ok () -> Format.sprintf "User %s created successfully!" name |> return
        | Error err -> Format.sprintf "%s\n" (Caqti_error.show err) |> return)
    | false -> Format.sprintf "Passwords don't match!\n" |> return
  in
  Format.sprintf "%s\n" response |> Lwt.return

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
  form_handler body >>= fun body -> Lwt.return body

let create_table (module Db : Caqti_lwt.CONNECTION) =
  let query =
    (unit ->. unit)
    @@ {| CREATE TABLE IF NOT EXISTS users (
        name VARCHAR(255) NOT NULL,
        email VARCHAR(255) PRIMARY KEY,
        password VARCHAR(255) NOT NULL
      )
  |}
  in
  Db.exec query

let drop_table (module Db : Caqti_lwt.CONNECTION) =
  let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS users |} in
  Db.exec query

let setup_db conn =
  let name = "johndoe" in
  let email = "johndoe@gmail.com" in
  let password = "johndoe" in
  create_table conn () >>= function
  | Ok _ -> (
      (*FIXME: 🤔This debug prints in setup_db are not printed!! *)
      Lwt_io.printf "Table created\n"
      >>= fun () ->
      Model.User.create_user conn name email password >>= function
      | Ok _ -> (
          (* Ensure that the user is created *)
          Model.User.find_user_by_email conn email
          >>= function
          | Ok (Some actual_name) ->
              Alcotest.(check string) "same name" name actual_name;
              Lwt.return @@ Ok "User found"
          | Ok None -> Alcotest.fail "User not found in the database!\n"
          | Error err -> Lwt.return @@ Error err (* Error while fetching user *)
          )
      | Error err -> Lwt.return @@ Error err (* Error while creating user *))
  | Error err -> Lwt.return (Error err)
(* error while creating table *)

let clear_db conn =
  drop_table conn () >>= function
  | Ok () -> Lwt_io.printf "Database is cleared\n" >>= fun () -> Lwt.return_unit
  | Error err ->
      Lwt.return
      @@ Format.printf "Error while clearing database: %s\n"
           (Caqti_error.show err)

let with_connection db_uri =
  Db.connect db_uri >>= function
  | Ok conn -> (
      (*FIXME: 🤔This debug print is not printed!! *)
      Lwt_io.printf "Connected to database\n"
      >>= fun () ->
      Lwt.finalize (fun () -> setup_db conn) (fun () -> clear_db conn)
      >>= function
      | Ok _ ->
          let expected_code = 200 in
          let expected_body = "User John Doe created successfully!\n" in
          body >>= fun body ->
          send_request `POST "/create_user" (Some body)
          >>= fun (actual_code, actual_body) ->
          Alcotest.(check int) "same status code" expected_code actual_code;
          Alcotest.(check string) "same body" expected_body actual_body;
          Lwt.return (Ok ())
      | Error err -> Lwt.return (Error err))
  | Error err -> Lwt.return (Error err)

let test_post_create_user _ () =
  with_connection db_uri >>= function
  | Ok _ -> Lwt_io.printf "Test completed successfully\n"
  | Error err ->
      Lwt.return
      @@ Format.printf "Error during testing: %s" (Caqti_error.show err)

let callback _conn req _body =
  let uri = Request.uri req in
  let meth = Request.meth req in
  let path = Uri.path uri in
  match (meth, App.split_path path) with
  | `GET, [] -> App.respond_ok Form.home
  | `GET, [ "signup" ] -> App.respond_ok Form.signup
  | `POST, [ "create_user" ] -> body >>= fun body -> App.respond_string body
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
