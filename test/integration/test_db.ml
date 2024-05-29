open Lwt.Infix

let db_uri = "TEST_DATABASE_URI"

let create_user name email password =
  Db.with_connection
    (fun conn -> Model.User.create_user conn name email password)
    db_uri

let get_user_by_email email =
  Db.with_connection (fun conn -> Model.User.find_user conn email) db_uri

let test_if_user_created _ () =
  let expected_name = "johndoe" in
  let email = "johndoe@gmail.com" in
  let password = "johndoe" in
  create_user expected_name email password >>= function
  | Ok _ -> (
      (* Ensure that the user is created *)
      get_user_by_email email >>= function
      | Ok (Some actual_name) ->
          Alcotest.(check string) "same name" expected_name actual_name;
          Lwt.return_unit
      | Ok None -> Alcotest.fail "User not found in the database!\n"
      | Error err ->
          Lwt_io.printf "Error fetching user: %s\n" (Caqti_error.show err))
  | Error err ->
      Alcotest.failf "Error creating user: %s\n" (Caqti_error.show err)

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ("create_user", [ test_case "Create user" `Quick test_if_user_created ]);
       ]
