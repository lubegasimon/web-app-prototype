open Tyxml
open Html

let title = title (txt "Expense tracker")

let visitor_home_page =
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

let user_home_page =
  let%html home =
    {|
        <div>
          Hello, you're now logged in!
        </div>
     |}
  in
  let logout_btn =
    form ~a:[ a_action "logout"; a_method `Post ] [ button [ txt "logout" ] ]
  in
  let change_password =
    form
      ~a:[ a_action "change_password"; a_method `Post ]
      [
        label
          [
            txt "Old password:";
            input ~a:[ a_name "old_password"; a_required () ] ();
          ];
        br ();
        br ();
        label
          [
            txt "New password:";
            input ~a:[ a_name "new_password"; a_required () ] ();
          ];
        br ();
        br ();
        button [ txt "change password" ];
      ]
  in
  html (head title []) (body [ home; change_password; logout_btn ])

let signup = html (head title []) (body [ Signup.form ])
