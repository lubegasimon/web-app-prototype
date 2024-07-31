open Caqti_type
open Caqti_request.Infix

module type Conn = Caqti_lwt.CONNECTION

let create_category (module Db : Conn) =
  let query =
    (tup3 string string string ->* tup3 string string string)
    @@ {|
    INSERT INTO categories (category_name, category_desc, date_created)
    VALUES (?, ?, ?) RETURNING *
  |}
  in
  Db.collect_list query

let find_all_categories (module Db : Conn) =
  let query = (unit ->! string) @@ {| SELECT category_name FROM categories |} in
  Db.find query

let update_category_name (module Db : Conn) new_name curr_name =
  let query =
    (tup2 string string ->* tup3 string string string)
    @@ {|
      UPDATE categories SET category_name = ? WHERE category_name = ? RETURNING *
    |}
  in
  Db.collect_list query (new_name, curr_name)
