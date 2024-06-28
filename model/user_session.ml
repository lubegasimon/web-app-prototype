module Q = struct
  open Caqti_request.Infix
  open Caqti_type

  let create =
    (tup3 string string string ->. unit)
      {|
        INSERT INTO sessions (session_id, csrf_token, user_email) VALUES (?, ?, ?)
      |}

  let delete =
    (string ->. unit) @@ {| DELETE FROM sessions WHERE session_id = ? |}

  let get_session =
    (string ->! tup2 string string)
    @@ {|
      SELECT csrf_token, user_email FROM sessions WHERE session_id = ?
    |}
end

let create_user_session (module Db : Caqti_lwt.CONNECTION) session_id csrf_token
    email =
  Db.exec Q.create (session_id, csrf_token, email)

let close_session (module Db : Caqti_lwt.CONNECTION) session_id =
  Db.exec Q.delete session_id

let get_session (module Db : Caqti_lwt.CONNECTION) session_id =
  Db.find_opt Q.get_session session_id
