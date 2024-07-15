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
  | Ok conn ->
      (*FIXME: 🤔This debug print is not printed!! *)
      Lwt_io.printf "Connected to database\n" >>= fun () ->
      Lwt.finalize (fun () -> setup_db conn) (fun () -> clear_db conn)
  | Error err -> Lwt.return (Error err)

let test_if_user_created _ () =
  with_connection db_uri >>= function
  | Ok _ -> Lwt_io.printf "Test completed successfully\n"
  | Error err ->
      Lwt.return
      @@ Format.printf "Error during testing: %s" (Caqti_error.show err)

let test_find_user_password_by_email _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      Model.User.find_user_password_by_email conn "johndoe@gmail.com"
      >>= function
      | Ok (Some password) ->
          Alcotest.(check string) "same password" password "johndoe";
          Lwt.return ()
      | Ok None -> Alcotest.fail "password not found!\n"
      | Error err -> Alcotest.fail (Caqti_error.show err))
  | Error err -> Alcotest.fail (Caqti_error.show err)

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ("create_user", [ test_case "Create user" `Quick test_if_user_created ]);
         ( "find user password",
           [
             test_case "find user password" `Quick
               test_find_user_password_by_email;
           ] );
       ]
