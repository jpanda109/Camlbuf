open Core.Std
open Target_pb

let print_str_as_bytes = 
  String.iter ~f:(fun c -> print_int (Char.to_int c); print_string " ")

let () =
  let a = A.({ a = 150 }) in
  A.encode a |> print_str_as_bytes; print_newline ();
  [8; 150; 1] |> List.map ~f:Char.of_int_exn |> String.of_char_list |> A.decode |> A.a |> print_int;
  print_newline ();
