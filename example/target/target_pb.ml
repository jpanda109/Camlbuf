(* this is essentially just an example of what I'd like the
   .proto files to compile to in ml 
*)
open Core.Std

module ByteStream : sig
  type t

  val read : t -> char option

end = struct
  type t =
    { arr: string;
      mutable i : int
    }

  let read t = None
end

let rec to_binary i =  (* returns list of ints representing binary, msb on left *)
  let cur = i mod 2 in
  let rest = i / 2 in
  match rest with
  | 0 -> [cur]
  | r -> cur::(to_binary r)

let to_dec bin =
  let rec f i mult =
    if i >= 0
    then ((List.nth_exn bin i) * mult) + (f (i-1) mult * 2)
    else 0
  in f ((List.length bin)-1) 1

let encode_varint tag i =
  let key = (tag lsl 3) lor 0 in
  let rec split_chunks bin =
    match List.split_n bin 7 with
    | (chunk, []) -> [0::chunk]
    | (chunk, rest) -> (1::chunk)::(split_chunks rest)
  in key::(to_binary i |> split_chunks |> List.map ~f:to_dec) |> List.map ~f:Char.of_int_exn |> String.of_char_list

let decode_varint bs =
  let rec get_bytes bs =  (*get relevant bits while removing msb *)
    match ByteStream.read bs with
    | Some b -> 
      let b = Char.to_int b in
      if (b land 0b1000000) = 1
      then (b land 0b01111111)::get_bytes bs
      else [b]
    | None -> []
  in get_bytes bs |> List.rev |> List.map ~f:to_binary |> List.concat |> to_dec

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
