open Redis_lwt
open Lwt

let redis_conn = Client.connect { host = "localhost"; port = 6379 }

(* TODO: Should the session have an expiration -- ... true -> Client.expire conn session_id 3600 *)
let create_session ~conn ~session_id ~is_authenticated ~email =
  let csrf_token = Header.csrf_token in
  let session_data =
    Yojson.to_string
    @@ `Assoc
         [
           ("authenticated", `Bool is_authenticated);
           ( "email",
             match email with Some email -> `String email | None -> `Null );
           ("csrf_token", `String csrf_token);
         ]
  in
  Client.set conn session_id session_data >>= function
  | true -> Lwt.return_ok csrf_token
  | false -> Lwt.return_ok "Failed to create session!"

let get_session ~conn ~session_id =
  Client.get conn session_id >>= function
  | Some session_data ->
      let json_data = Yojson.Safe.from_string session_data in
      Lwt.return (Some json_data)
  | None -> Lwt.return None

let validate_csrf_token ~conn ~session_id ~actual_token =
  get_session ~conn ~session_id >>= function
  | Some session_data -> (
      match session_data with
      | `Assoc data ->
          let expected_token =
            List.assoc "csrf_token" data |> Yojson.Safe.to_string
          in
          Lwt.return (expected_token = actual_token)
      | _ -> Lwt.return false)
  | _ -> Lwt.return false

let delete_session ~conn ~session_id =
  Client.del conn [ session_id ] >>= fun res ->
  match res > 0 with
  | true -> Lwt.return_ok "Session deleted"
  | false -> Lwt.return_ok "No session is deleted"
