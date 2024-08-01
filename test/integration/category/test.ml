open Lwt.Infix

let db_uri = "TEST_DATABASE_URI"
let database_error err = Alcotest.fail (Caqti_error.show err)

let test_find_all_categories _ () =
  Db.connect db_uri >>= function
  | Ok conn -> (
      Model.Category.find_all_categories conn () >>= function
      | Ok name ->
          Alcotest.(check string) "should be same name" name "Utility"
          |> Lwt.return
      | Error err -> database_error err)
  | Error err -> database_error err

let test_update_category_name _ () =
  Db.connect db_uri >>= function
  | Ok conn ->
      Lwt.finalize
        (fun () ->
          let curr_name = "Utility" in
          let new_name = "Utilities" in
          Model.Category.update_category_name conn new_name curr_name
          >>= function
          | Ok result ->
              Alcotest.(check (list (triple string string string)))
                "should return with updated category data" result
                [ ("Utilities", "Utility e.g water bills", "2024-07-29") ];
              Lwt.return ()
          | Error err -> database_error err)
        (fun () -> Category.Helper.cleanup_db conn)
  | Error err -> database_error err

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Database queries"
       [
         ( "test: create_category",
           [
             test_case "Create category" `Quick Category.Helper.create_category;
           ] );
         ( "test: fetch categories",
           [ test_case "fetch categories" `Quick test_find_all_categories ] );
         ( "test: update category name",
           [ test_case "update category name" `Quick test_update_category_name ]
         );
       ]
