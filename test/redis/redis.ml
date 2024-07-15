open Middleware.Redis
open Lwt

let create_session_test _ () =
  let session_id = "1234" in
  let expected = "00000000-0000-4000-8000-000000000000" in
  redis_conn >>= fun conn ->
  create_session ~conn ~session_id ~is_authenticated:false ~email:None
  >>= function
  | Ok token ->
      Alcotest.(check string) "same result" token expected;
      Lwt.return_unit
  | Error err -> Alcotest.fail err

let get_session_test _ () =
  let session_id = "1234" in
  let expected =
    "{\"authenticated\":false,\"email\":null,\"csrf_token\":\"00000000-0000-4000-8000-000000000000\"}"
  in
  redis_conn >>= fun conn ->
  get_session ~conn ~session_id >>= function
  | Some data ->
      Alcotest.(check string) "same id" (Yojson.Safe.to_string data) expected;
      Lwt.return_unit
  | None -> Alcotest.fail "Failed to fetch session"

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "redis session tests"
       [
         ( "create session",
           [ test_case "create sesion" `Quick create_session_test ] );
         ( "get session id",
           [ test_case "get sesion id" `Quick get_session_test ] );
       ]
