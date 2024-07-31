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

let cleanup_db (module Db : Caqti_lwt.CONNECTION) =
  let drop_table =
    let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS categories |} in
    Db.exec query
  in
  drop_table () >>= function
  | Ok _ -> Lwt.return ()
  | Error err -> database_error err

let test_create_category _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_table conn () >>= function
      | Ok _ -> (
          let category_name = "Yaka" in
          let category_desc = "Electricity bill" in
          let date_created = "07/29/24" in
          Model.Category.create_category conn
            (category_name, category_desc, date_created)
          >>= function
          | Ok result ->
              Alcotest.(check (list (triple string string string)))
                "should return category data" result
                [ ("Yaka", "Electricity bill", "2024-07-29") ];
              Lwt.return ()
          | Error err -> database_error err)
      | Error err -> database_error err)
  | Error err -> database_error err

let test_find_all_categories _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      Model.Category.find_all_categories conn () >>= function
      | Ok name ->
          Alcotest.(check string) "should be same name" name "Yaka"
          |> Lwt.return
      | Error err -> database_error err)
  | Error err -> database_error err

let test_update_category_name _ () =
  Db.connect db_uri >>= function
  | Ok conn ->
      Lwt.finalize
        (fun () ->
          let curr_name = "Yaka" in
          let new_name = "yaka" in
          Model.Category.update_category_name conn new_name curr_name
          >>= function
          | Ok [ (name, desc, date) ] ->
              Alcotest.(check (list string))
                "should return with updated category data"
                (name :: desc :: [ date ])
                [ "yaka"; "Electricity bill"; "2024-07-29" ];
              Lwt.return ()
          | Ok _ -> Alcotest.fail "Query returned unexpected rows!\n"
          | Error err -> database_error err)
        (fun () -> cleanup_db conn)
  | Error err -> database_error err

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ( "test: create_category",
           [ test_case "Create category" `Quick test_create_category ] );
         ( "test: fetch categories",
           [ test_case "fetch categories" `Quick test_find_all_categories ] );
         ( "test: update category name",
           [ test_case "update category name" `Quick test_update_category_name ]
         );
       ]
