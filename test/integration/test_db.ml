open Lwt.Infix

module To_test = struct
  let db_uri = "TEST_DATABASE_URI"

  let create_user name email password =
    Db.with_connection
      (fun conn -> Model.User.create_user conn name email password)
      db_uri

  let get_user_by_email email =
    Db.with_connection (fun conn -> Model.User.find_user conn email) db_uri
end

(* The tests *)
let test_if_user_created _ () =
  let expected = () in
  let actual =
    To_test.create_user "johndoe" "johndoe@gmail.com" "johndoe" >>= function
    | Ok _ -> (
        (* Ensure that the user is created *)
        To_test.get_user_by_email "johndoe@gmail.com"
        >>= function
        | Ok (Some user_name) ->
            Lwt_io.printf "User '%s' successfully created\n" user_name
        | Ok None -> Lwt_io.printf "User not found in the database!\n"
        | Error err -> Lwt_io.printf "%s\n" (Caqti_error.show err))
    | Error err ->
        Lwt_io.printf "Error fetching user: %s\n" (Caqti_error.show err)
  in
  actual >|= Alcotest.(check unit) "user" expected

(* Run it *)
let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ("create_user", [ test_case "Create user" `Quick test_if_user_created ]);
       ]
