open Lwt.Infix

module To_test = struct
  let create_user name email password =
    Db.with_connection
      (fun conn -> Model.User.create_user conn name email password)
      "TEST_DATABASE_URI"
end

(* The tests *)
let test_if_user_created _ () =
  let expected = () in
  let actual =
    To_test.create_user "johndoe" "johndoe@gmail.com" "johndoe" >>= function
    | Ok _ -> Lwt.return_unit
    | Error err ->
        Lwt_io.printf "Error creating user: %s\n" (Caqti_error.show err)
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
