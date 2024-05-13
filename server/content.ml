open Tyxml

let to_ocaml = Html.(a ~a:[ a_href "ocaml.org" ] [ txt "Welcome to OCaml!" ])
let ocaml_world = Html.(a [ txt "You're now in the OCaml universe!" ])
let hello = Html.(a ~a:[ a_href "hello" ] [ txt "Hello, World!" ])
