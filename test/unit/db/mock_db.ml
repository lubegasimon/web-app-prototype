(* A module with functions to test *)
(* Mock database *)
module User = struct
  type user = { name : string; email : string; password : string }
  [@@deriving eq]

  let create_user name email password = { name; email; password }
end

let pp_user fmt user =
  let { User.name; email; password } = user in
  Format.fprintf fmt "{name: %s; email: %s; password: %s}" name email password

let users_equal user1 user2 = User.equal_user user1 user2
let testable_user = Alcotest.testable pp_user users_equal

(* The tests *)
let test_user () =
  let expected =
    { User.name = "John"; email = "johndoe@yahoo.com"; password = "johndoe" }
  in
  let actual = User.create_user "John" "johndoe@yahoo.com" "johndoe" in
  Alcotest.(check testable_user) "same user" expected actual

(* Run it *)
let () =
  let open Alcotest in
  run "database query logic"
    [ ("create_user", [ test_case "create user" `Quick test_user ]) ]
