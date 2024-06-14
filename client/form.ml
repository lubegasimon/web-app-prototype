open Tyxml
open Html

let title = title (txt "Expense tracker")

let home =
  let%html home =
    {|
      <div>
        Hello, Welcome to expense tracker!
        <br>
        <div><a href = 'signup'> Sign up </a> </div>
      </div>
   |}
  in
  html (head title []) (body [ home ])

let signup = html (head title []) (body [ Signup.form ])
