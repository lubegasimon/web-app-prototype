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

let create_user =
  let name = "johndoe" in
  let email = "johndoe@gmail.com" in
  let password = "johndoe" in
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_table conn () >>= function
      | Ok _ -> (
          Model.User.create_user conn (name, email, password) >>= function
          | Ok _ -> Lwt.return ()
          | Error err -> database_error err)
      | Error err -> database_error err)
  | Error err -> database_error err

let test_create_user _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_user >>= function
      | () -> (
          Model.User.find_user_by_email conn "johndoe@gmail.com" >>= function
          | Ok (Some actual_name) ->
              Alcotest.(check string) "same name" actual_name "johndoe";
              Lwt.return ()
          | Ok None -> Alcotest.fail "User not found in the database!\n"
          | Error err -> database_error err))
  | Error err -> database_error err

let drop_table (module Db : Caqti_lwt.CONNECTION) =
  let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS users |} in
  Db.exec query

let clean_up conn =
  drop_table conn () >>= function
  | Ok _ -> Lwt.return ()
  | Error err -> database_error err

let test_find_user_password_by_email _ () =
  Db.connect db_uri >>= function
  | Ok conn ->
      Lwt.finalize
        (fun () ->
          create_user >>= function
          | () -> (
              Model.User.find_user_password_by_email conn "johndoe@gmail.com"
              >>= function
              | Ok (Some password) ->
                  Alcotest.(check string) "same password" password "johndoe";
                  Lwt.return ()
              | Ok _ -> Alcotest.fail "password not found!\n"
              | Error err -> database_error err))
        (fun () -> clean_up conn)
  | Error err -> database_error err

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ("create_user", [ test_case "Create user" `Quick test_create_user ]);
         ( "find user password",
           [
             test_case "find user password" `Quick
               test_find_user_password_by_email;
           ] );
       ]
