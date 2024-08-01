open Lwt.Infix
open Caqti_type
open Caqti_request.Infix

let db_uri = "TEST_DATABASE_URI"
let database_error err = Alcotest.fail (Caqti_error.show err)

(* create and intialize categories table because it is referenced by expenses table *)
let init_categories_tbl = Category.Helper.create_category

let create_table (module Db : Caqti_lwt.CONNECTION) =
  let query =
    (unit ->. unit)
    @@ {| CREATE TABLE IF NOT EXISTS expenses (
        expense_name VARCHAR(255) PRIMARY KEY,
        expense_desc VARCHAR(255),
        category VARCHAR(255) REFERENCES categories ON DELETE RESTRICT,
        date_created date
      )
  |}
  in
  Db.exec query

let cleanup_db (module Db : Caqti_lwt.CONNECTION) =
  let drop_table =
    let query = (unit ->. unit) @@ {| DROP TABLE IF EXISTS expenses |} in
    Db.exec query
  in
  drop_table () >>= function
  | Ok _ -> Lwt.return ()
  | Error err -> database_error err

let test_create_expense _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      create_table conn () >>= function
      | Ok _ -> (
          let expense_name = "Yaka" in
          let expense_desc = "Electricity bill" in
          let category = "Utility" in
          let date_created = "07/29/24" in
          Model.Expense.create_expense conn
            (expense_name, expense_desc, category, date_created)
          >>= function
          | Ok [ (name, desc, category, date) ] ->
              Alcotest.(check (list string))
                "should return category data"
                (name :: desc :: category :: [ date ])
                [ "Yaka"; "Electricity bill"; "Utility"; "2024-07-29" ];
              Lwt.return ()
          | Ok _ -> Alcotest.fail "Query returned unexpected results\n"
          | Error err -> database_error err)
      | Error err -> database_error err)
  | Error err -> database_error err

let test_find_all_expenses _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      Model.Expense.find_all_expenses conn () >>= function
      | Ok name ->
          Alcotest.(check string) "should be same name" name "Yaka"
          |> Lwt.return
      | Error err -> database_error err)
  | Error err -> database_error err

let test_update_expense_name _ () =
  Db.connect db_uri >>= function
  | Ok conn ->
      Lwt.finalize
        (fun () ->
          let curr_name = "Yaka" in
          let new_name = "yaka" in
          Model.Expense.update_expense_name conn new_name curr_name >>= function
          | Ok [ (name, desc, category, date) ] ->
              Alcotest.(check (list string))
                "should return category data"
                (name :: desc :: category :: [ date ])
                [ "yaka"; "Electricity bill"; "Utility"; "2024-07-29" ];
              Lwt.return ()
          | Ok _ -> Alcotest.fail "Query returned unexpected results\n"
          | Error err -> database_error err)
          (* drop expenses table first, then categories the former references the latter *)
        (fun () ->
          cleanup_db conn >>= fun () -> Category.Helper.cleanup_db conn)
  | Error err -> database_error err

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ( "test: initialize categories table",
           [
             test_case "initialize categories table" `Quick init_categories_tbl;
           ] );
         ( "test: create_expense",
           [ test_case "Create expense" `Quick test_create_expense ] );
         ( "test: fetch expenses",
           [ test_case "fetch expenses" `Quick test_find_all_expenses ] );
         ( "test: update expense name",
           [ test_case "update expense name" `Quick test_update_expense_name ]
         );
       ]
