(* TODO: write unit test *)
let field_required field value =
  match value <> "" with
  | true -> Ok true
  | false -> Error (Error.to_string (Required_field field))

let validate_form form field =
  let field_value field =
    match List.assoc_opt field form with
    | Some x -> List.hd x |> String.trim
    | None -> Error.to_string (Empty_field field)
  in
  let value = field_value field in
  match field_required field value with Ok _ -> Ok value | Error e -> Error e
