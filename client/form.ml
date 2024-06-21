open Tyxml
open Html

let title = title (txt "Expense tracker")

let site_home =
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

let user_home =
  let%html home =
    {|
        <div>
          Hello, you're now logged in!
        </div>
     |}
  in
  html (head title []) (body [ home ])

let signup = html (head title []) (body [ Signup.form ])
