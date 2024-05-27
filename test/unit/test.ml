(* A module with functions to test *)
module To_test = struct
  open App

  let split_path = split_path
  let field_values = field_values
end

let pp_signup_form fmt form =
  let { App.name; email; password; confirm_password } = form in
  Format.fprintf fmt "{name: %s; email: %s; password: %s; confirm_password: %s}"
    name email password confirm_password

let equal_form form1 form2 = App.equal_signup_form form1 form2
let signup_form_testable = Alcotest.testable pp_signup_form equal_form

(* The tests *)
let test_split_path () =
  let expected = [ "http:"; "localhost:8000"; "signup" ] in
  let actual = To_test.split_path "http://localhost:8000/signup/" in
  Alcotest.(check (list string)) "same lists" expected actual

let test_field_values () =
  let expected =
    {
      App.name = "John";
      email = "johndoe@gmail.com";
      password = "johndoe";
      confirm_password = "johndoe";
    }
  in
  let actual =
    To_test.field_values
      [
        ("name", [ "John" ]);
        ("email", [ "johndoe@gmail.com" ]);
        ("password", [ "johndoe" ]);
        ("confirm_password", [ "johndoe" ]);
      ]
  in
  Alcotest.(check signup_form_testable) "same form" expected actual

(* Run it *)
let () =
  let open Alcotest in
  run "Functions"
    [
      ("split_path_case", [ test_case "Split path" `Quick test_split_path ]);
      ( "field_values_case",
        [ test_case "Field values" `Quick test_field_values ] );
    ]
