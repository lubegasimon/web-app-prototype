let session_id = Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string
let csrf_token = Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string

let headers =
  Cohttp.Header.of_list
    [
      ("Set-Cookie", Format.sprintf "session_id=%s; HttpOnly; secure" session_id);
      ("X-Csrf-token", Format.sprintf "%s" csrf_token);
    ]
