open Caqti_type
open Caqti_request.Infix

module type Conn = Caqti_lwt.CONNECTION

let create_user (module Db : Conn) =
  let query =
    (tup3 string string string ->* tup3 string string string)
    @@ {|
    INSERT INTO users (name, email, password)
    VALUES (?, ?, ?) RETURNING *
  |}
  in
  Db.collect_list query

let find_user_by_email (module Db : Conn) =
  let query =
    (string ->* tup2 string string)
    @@ {| SELECT name, password FROM users WHERE email = ? |}
  in
  Db.collect_list query

let update_user_password (module Db : Conn) new_password email =
  let query =
    (tup2 string string ->* tup3 string string string)
    @@ {|
      UPDATE users SET password = ? WHERE email = ? RETURNING *
    |}
  in
  Db.collect_list query (new_password, email)
