open Lwt.Infix
open Caqti_request.Infix

module Q = struct
  let create_table =
    let open Caqti_type in
    (unit ->. unit)
    @@ "CREATE TABLE accs (id text NOT NULL, name text NOT NULL)"

  let drop_table =
    let open Caqti_type in
    (unit ->. unit) @@ "DROP TABLE users"
end

let create_table (module Db : Caqti_lwt.CONNECTION) = Db.exec Q.create_table
let drop_table (module Db : Caqti_lwt.CONNECTION) = Db.exec Q.drop_table

let report_error = function
  | Ok () -> Lwt_io.printf "Database connection is successful!\n"
  | Error err ->
      Lwt_io.printf "Failed to connect to database: %s\n" (Caqti_error.show err)

let use_connection (module Conn : Caqti_lwt.CONNECTION) =
  let%lwt () = Lwt_io.printf "Creating table...\n" in
  let%lwt _ =
    match%lwt create_table (module Conn) () with
    | Ok () -> Lwt.return (Ok (Printf.printf "Table successfully created!\n"))
    | Error err -> Lwt.return (Error err)
  in
  let%lwt () = Lwt_io.printf "Dropping table...\n" in
  match%lwt drop_table (module Conn) () with
  | Ok () -> Lwt.return (Ok (Printf.printf "Table successfully dropped!\n"))
  | Error err -> Lwt.return (Error err)

let uri =
  match Sys.getenv_opt "DATABASE_URI" with
  | Some uri -> Uri.of_string uri
  | None -> Uri.of_string "Postgresql://"

let main =
  Lwt_main.run (Caqti_lwt.with_connection uri use_connection >>= report_error)

let main_cmd =
  let open Cmdliner in
  let doc = "Web server prototype" in
  let term = Term.(const main) in
  let info = Cmd.info ~doc "web-server-prototype" in
  Cmd.v info term

let () = exit (Cmdliner.Cmd.eval main_cmd)
