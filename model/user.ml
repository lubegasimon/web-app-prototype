module Q = struct
  open Caqti_type
  open Caqti_request.Infix

  let create_table =
    (unit ->. unit)
    @@ {|
      CREATE TABLE users (
        name VARCHAR(255) NOT NULL,
        email VARCHAR(255) PRIMARY KEY,
        password VARCHAR(255) NOT NULL
      )
    |}

  let drop_table = (unit ->. unit) @@ "DROP TABLE accs"

  let create_user =
    (tup3 string string string ->. unit)
    @@ {|
    INSERT INTO users (name, email, password)
    VALUES (?, ?, ?)
  |}
end

let create_table (module Db : Caqti_lwt.CONNECTION) = Db.exec Q.create_table
let drop_table (module Db : Caqti_lwt.CONNECTION) = Db.exec Q.drop_table

let create_user (module Db : Caqti_lwt.CONNECTION) name email password =
  Db.exec Q.create_user (name, email, password)
