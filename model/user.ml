module Q = struct
  open Caqti_type
  open Caqti_request.Infix

  let create_user =
    (tup3 string string string ->. unit)
    @@ {|
    INSERT INTO users (name, email, password)
    VALUES (?, ?, ?)
  |}
end

let create_user (module Db : Caqti_lwt.CONNECTION) name email password =
  Db.exec Q.create_user (name, email, password)
