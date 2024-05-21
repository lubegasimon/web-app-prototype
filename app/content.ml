open Tyxml
open Html

let form =
  form
    ~a:[ a_action "add_user"; a_method `Post ]
    [
      fieldset
        [
          div [ txt "Create User" ];
          label [ txt "ID:"; input ~a:[ a_name "id"; a_required () ] () ];
          br ();
          br ();
          label [ txt "Name:"; input ~a:[ a_name "name"; a_required () ] () ];
          br ();
          br ();
          button [ txt "Submit" ];
        ];
    ]

let%html home = "<a href = 'signup'> Hello, Welcome to expense tracker! </a>"
let%html signup = "<a href = 'create_acc'> Sign up! </a>"
let title = title (txt "Expense tracker")
let create_user = html (head title []) (body [ form ])
