open Tyxml
open Html

let title = title (txt "Expense tracker")

let%html home =
  {|
<div>
  Hello, Welcome to expense tracker!
  <br>
  <a href = 'signup'> Sign up </a>
</div>
|}

let signup = html (head title []) (body [ Signup.form ])
