module Q = struct
  open Caqti_type
  open Caqti_request.Infix

  let create_user =
    (tup3 string string string ->. unit)
    @@ {|
    INSERT INTO users (name, email, password)
    VALUES (?, ?, ?)
  |}

  let find_user_by_email =
    (string ->! string) @@ {| SELECT name FROM users WHERE email = ? |}

  let find_user_password_by_email =
    (string ->! string) @@ {| SELECT password FROM users WHERE email = ? |}

  let update_user =
    (tup2 string string ->. unit)
    @@ {|
      UPDATE users SET password = ? WHERE email = ?
    |}
end

let create_user (module Db : Caqti_lwt.CONNECTION) name email password =
  Db.exec Q.create_user (name, email, password)

let find_user_by_email (module Db : Caqti_lwt.CONNECTION) =
  Db.find_opt Q.find_user_by_email

let find_user_password_by_email (module Db : Caqti_lwt.CONNECTION) =
  Db.find_opt Q.find_user_password_by_email

let update_user (module Db : Caqti_lwt.CONNECTION) new_password email =
  Db.exec Q.update_user (new_password, email)
