open Core.Std
open Target_pb

let print_str_as_bytes = 
  String.iter ~f:(fun c -> print_int (Char.to_int c); print_string " ")

let print_a { A.a = a; b = b; c = c } =
  String.concat ["{ "; String.concat ~sep:"; " [Int.to_string a; b; A.Enum.to_string c];  " }"]
  |> print_string

let () =
  let a = A.({ a = 150; b = "aa"; c = Enum.B }) in
  A.encode a |> print_str_as_bytes; print_newline ();
  [8; 150; 1; 18; 3; 97; 98; 99; 24; 1] |> List.map ~f:Char.of_int_exn |> String.of_char_list |> A.decode |> print_a;
  print_newline ();
