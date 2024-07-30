open Caqti_type
open Caqti_request.Infix

module type Conn = Caqti_lwt.CONNECTION

let create_user (module Db : Conn) =
  let query =
    (tup3 string string string ->. unit)
    @@ {|
    INSERT INTO users (name, email, password)
    VALUES (?, ?, ?)
  |}
  in
  Db.exec query

let find_user_by_email (module Db : Conn) =
  let query =
    (string ->! string) @@ {| SELECT name FROM users WHERE email = ? |}
  in
  Db.find_opt query

let find_user_password_by_email (module Db : Conn) =
  let query =
    (string ->! string) @@ {| SELECT password FROM users WHERE email = ? |}
  in
  Db.find_opt query

let update_user_password (module Db : Conn) new_password email =
  let query =
    (tup2 string string ->. unit)
    @@ {|
      UPDATE users SET password = ? WHERE email = ?
    |}
  in
  Db.exec query (new_password, email)
