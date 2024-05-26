open Tyxml
open Html

let form =
  form
    ~a:[ a_action "create_user"; a_method `Post ]
    [
      fieldset
        [
          label [ txt "Name:"; input ~a:[ a_name "name"; a_required () ] () ];
          br ();
          br ();
          label [ txt "Email:"; input ~a:[ a_name "email"; a_required () ] () ];
          br ();
          br ();
          label
            [
              txt "Password:"; input ~a:[ a_name "password"; a_required () ] ();
            ];
          br ();
          br ();
          label
            [
              txt "Confirm password:";
              input ~a:[ a_name "confirm_password"; a_required () ] ();
            ];
          br ();
          br ();
          button [ txt "Submit" ];
        ];
    ]
