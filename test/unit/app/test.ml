open Handler

let pp_signup_form fmt form =
  let { Signup.name; email; password; confirm_password } = form in
  Format.fprintf fmt "{name: %s; email: %s; password: %s; confirm_password: %s}"
    name email password confirm_password

let equal_form form1 form2 = Signup.equal_signup_form form1 form2
let signup_form_testable = Alcotest.testable pp_signup_form equal_form

let test_sanitize_path () =
  let expected = [ "http:"; "localhost:8000"; "signup" ] in
  let actual = App.sanitize_path "http://localhost:8000/signup/" in
  Alcotest.(check (list string)) "same lists" expected actual

let test_field_values () =
  let name = "John" in
  let email = "johndoe@yahoo.com" in
  let password = "johndoe" in
  let expected =
    { Signup.name; email; password; confirm_password = password }
  in
  let actual =
    Signup.validate_form
      [
        ("name", [ name ]);
        ("email", [ email ]);
        ("password", [ password ]);
        ("confirm_password", [ password ]);
      ]
  in
  match actual with
  | Ok actual ->
      Alcotest.(check signup_form_testable) "same form" expected actual
  | Error err -> raise (failwith err)

let () =
  let open Alcotest in
  run "Functions"
    [
      ( "sanitize_path_case",
        [ test_case "Split path" `Quick test_sanitize_path ] );
      ( "field_values_case",
        [ test_case "Field values" `Quick test_field_values ] );
    ]
