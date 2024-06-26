module Q = struct
  open Caqti_request.Infix
  open Caqti_type

  let create =
    (tup3 string string string ->. unit)
      {|
        INSERT INTO sessions (session_id, csrf_token, user_email) VALUES (?, ?, ?)
      |}
end

let create_user_session (module Db : Caqti_lwt.CONNECTION) session_id csrf_token
    email =
  Db.exec Q.create (session_id, csrf_token, email)
