type error =
  | Empty_field of string
  | Required_field of string
  | Invalid_field of string
  | Email_used
  | Database_error of Caqti_error.t
  | Password_mismatch

let to_string = function
  | Empty_field field -> Format.sprintf "Field %s is empty!" field
  | Required_field field -> Format.sprintf "Field %s is required!" field
  | Invalid_field field -> Format.sprintf "Field %s is invalid!" field
  | Email_used -> "Email is already used"
  | Database_error err ->
      Format.sprintf "Database error: %s\n" (Caqti_error.show err)
  | Password_mismatch -> "Passwords don't match!"
