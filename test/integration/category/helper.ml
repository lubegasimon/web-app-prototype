open Lwt.Infix
open Caqti_type
open Caqti_request.Infix

let db_uri = "TEST_DATABASE_URI"
let database_error err = Alcotest.fail (Caqti_error.show err)

let create_table (module Db : Caqti_lwt.CONNECTION) =
  let query =
    (unit ->. unit)
    @@ {| CREATE TABLE IF NOT EXISTS categories (
        category_name VARCHAR(255) PRIMARY KEY,
        category_desc VARCHAR(255),
        date_created date
      )
  |}
  in
  Db.exec query

let create_category _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_table conn () >>= function
      | Ok _ -> (
          let category_name = "Utility" in
          let category_desc = "Utility e.g water bills" in
          let date_created = "07/29/24" in
          Model.Category.create_category conn
            (category_name, category_desc, date_created)
          >>= function
          | Ok result ->
              Alcotest.(check (list (triple string string string)))
                "should return category data" result
                [ ("Utility", "Utility e.g water bills", "2024-07-29") ];
              Lwt.return ()
          | Error err -> database_error err)
      | Error err -> database_error err)
  | Error err -> database_error err

let cleanup_db (module Db : Caqti_lwt.CONNECTION) =
  let drop_table =
    let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS categories |} in
    Db.exec query
  in
  drop_table () >>= function
  | Ok _ -> Lwt.return ()
  | Error err -> database_error err
