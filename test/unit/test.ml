(* A module with functions to test *)
module To_test = struct
  let split_path = App.split_path
end

(* The tests *)
let test_split_path () =
  Alcotest.(check (list string))
    "same lists"
    [ "http:"; "localhost:8000"; "signup" ]
    (To_test.split_path "http://localhost:8000/signup/")

(* Run it *)
let () =
  let open Alcotest in
  run "Functions"
    [ ("split_path_case", [ test_case "Split path" `Quick test_split_path ]) ]
