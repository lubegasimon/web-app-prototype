open Tyxml
open Html

let form csrf_token =
  form
    ~a:[ a_action "signup"; a_method `Post ]
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
              txt "Password:";
              input
                ~a:[ a_name "password"; a_input_type `Password; a_required () ]
                ();
            ];
          br ();
          br ();
          label
            [
              txt "Confirm password:";
              input
                ~a:
                  [
                    a_name "confirm_password";
                    a_input_type `Password;
                    a_required ();
                  ]
                ();
            ];
          br ();
          input
            ~a:[ a_input_type `Hidden; a_name "csrf_token"; a_value csrf_token ]
            ();
          br ();
          button ~a:[ a_button_type `Submit ] [ txt "Submit" ];
        ];
    ]
