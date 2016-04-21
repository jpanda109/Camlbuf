open Target_pb

let () =
  let a = A.({ a = 150 }) in
  A.encode a |> print_endline
