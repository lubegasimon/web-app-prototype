let session_id = Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string
let csrf_token = Uuidm.v4 (Bytes.create 16) |> Uuidm.to_string

let headers =
  Cohttp.Header.of_list
    [
      ("Set-Cookie", Format.sprintf "session_id=%s" session_id);
      ("Set-Cookie", Format.sprintf "csrf-token=%s" csrf_token);
    ]
