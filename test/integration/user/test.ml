open Lwt.Infix
open Caqti_type
open Caqti_request.Infix

let db_uri = "TEST_DATABASE_URI"

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

let database_error err = Alcotest.fail (Caqti_error.show err)

let test_create_user _ () =
  let name = "johndoe" in
  let email = "johndoe@gmail.com" in
  let password = "john_doe" in
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_table conn () >>= function
      | Ok _ -> (
          Model.User.create_user conn (name, email, password) >>= function
          | Ok result ->
              Alcotest.(check (list (triple string string string)))
                "should return same user data" result
                [ ("johndoe", "johndoe@gmail.com", "john_doe") ];
              Lwt.return ()
          | Error err -> database_error err)
      | Error err -> database_error err)
  | Error err -> database_error err

let test_find_user_by_email _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      Model.User.find_user_by_email conn "johndoe@gmail.com" >>= function
      | Ok result ->
          Alcotest.(check (list (pair string string)))
            "should be same password" result
            [ ("johndoe", "john_doe") ];
          Lwt.return ()
      | Error err -> database_error err)
  | Error err -> database_error err

let cleanup_db (module Db : Caqti_lwt.CONNECTION) =
  let drop_table =
    let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS users |} in
    Db.exec query
  in
  drop_table () >>= function
  | Ok _ -> Lwt.return ()
  | Error err -> database_error err

let test_update_user_password _ () =
  Db.connect db_uri >>= function
  | Ok conn ->
      Lwt.finalize
        (fun () ->
          let new_password = "doe_john" in
          let user_email = "johndoe@gmail.com" in
          Model.User.update_user_password conn new_password user_email
          >>= function
          | Ok result ->
              Alcotest.(check (list (triple string string string)))
                "should return with updated data" result
                [ ("johndoe", "johndoe@gmail.com", "doe_john") ];
              Lwt.return ()
          | Error err -> database_error err)
        (fun () -> cleanup_db conn)
  | Error err -> database_error err

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ( "tests: create_user",
           [ test_case "create user" `Quick test_create_user ] );
         ( "test: find user by email",
           [ test_case "find user by email" `Quick test_find_user_by_email ] );
         ( "test: update user password",
           [ test_case "update user password" `Quick test_update_user_password ]
         );
       ]
