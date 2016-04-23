open Core.Std
open Target_pb

let print_str_as_bytes = 
  String.iter ~f:(fun c -> print_int (Char.to_int c); print_string " ")

let () =
  let a = A.({ a = 150 }) in
  A.encode a |> print_str_as_bytes; print_newline ()
