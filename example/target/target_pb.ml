(* this is essentially just an example of what I'd like the
   .proto files to compile to in ml 
*)
open Core.Std

let encode_varint tag i =
  let key = (tag lsl 3) lor 0 in
  let rec to_binary i =  (* returns list of ints representing binary, msb on left *)
    let cur = i mod 2 in
    let rest = i / 2 in
    match rest with
    | 0 -> [cur]
    | r -> cur::(to_binary r)
  in
  let rec split_chunks bin =
    match List.split_n bin 7 with
    | (chunk, []) -> [0::chunk]
    | (chunk, rest) -> (1::chunk)::(split_chunks rest)
  in
  let to_dec bin =
    let rec f i mult =
      if i >= 0
      then ((List.nth_exn bin i) * mult) + (f (i-1) mult * 2)
      else 0
    in f ((List.length bin)-1) 1
  in key::(to_binary i |> split_chunks |> List.map ~f:to_dec) |> List.map ~f:Char.of_int_exn |> String.of_char_list

module rec A : sig
  type t =
    { a: int;
    }

  val encode : t -> string

end = struct

  type t =
    { a: int;
    } 

  let encode t =
    let encode_a = encode_varint 1 t.a in
    encode_a

end
