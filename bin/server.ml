let () =
  Middleware.Log.log;
  Lwt_main.run App.server
