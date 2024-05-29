(* Mock database module *)

type user = { name : string; email : string; password : string } [@@deriving eq]

let create_user name email password = { name; email; password }

let pp_user fmt user =
  let { name; email; password } = user in
  Format.fprintf fmt "{name: %s; email: %s; password: %s}" name email password

let users_equal user1 user2 = equal_user user1 user2
let testable_user = Alcotest.testable pp_user users_equal

let test_user () =
  let name = "John" in
  let email = "johndoe@yahoo.com" in
  let password = "johndoe" in
  let expected = { name; email; password } in
  let actual = create_user name email password in
  Alcotest.(check testable_user) "same user" expected actual

let () =
  let open Alcotest in
  run "mock database query"
    [ ("create_user", [ test_case "create user" `Quick test_user ]) ]
