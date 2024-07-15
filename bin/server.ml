open Lwt

let () =
  Middleware.Log.log;
  Lwt_main.run
    (Lwt_io.printf "Application starting...\n" >>= fun () -> App.server)
