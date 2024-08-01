open Caqti_type
open Caqti_request.Infix

module type Conn = Caqti_lwt.CONNECTION

let create_expense (module Db : Conn) =
  let query =
    (tup4 string string string string ->* tup4 string string string string)
    @@ {|
    INSERT INTO expenses (expense_name, expense_desc, category, date_created)
    VALUES (?, ?, ?, ?) RETURNING *
  |}
  in
  Db.collect_list query

let find_all_expenses (module Db : Conn) =
  let query = (unit ->! string) @@ {| SELECT expense_name FROM expenses |} in
  Db.find query

let update_expense_name (module Db : Conn) new_name curr_name =
  let query =
    (tup2 string string ->* tup4 string string string string)
    @@ {|
      UPDATE expenses SET expense_name = ? WHERE expense_name = ? RETURNING *
    |}
  in
  Db.collect_list query (new_name, curr_name)
