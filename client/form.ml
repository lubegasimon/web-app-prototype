open Tyxml
open Html

let title = title (txt "Expense tracker")

let home message =
  let%html header =
    {|
      <div>
        Hello, Welcome to expense tracker!
      </div>
   |}
  in
  html (head title [])
    (body
       [
         header;
         (match message with
         | Some msg -> p [ txt msg ]
         | None ->
             let%html signup =
               {|<div><a href = 'signup'> Sign up </a> </div>|}
             in
             signup);
       ])

let signup error =
  html (head title [])
    (body
       [
         (match error with
         | Some err -> p ~a:[ a_style "color: red;" ] [ txt err ]
         | None -> Signup.form);
       ])
