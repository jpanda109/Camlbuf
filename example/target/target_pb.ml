(* this is essentially just an example of what I'd like the
   .proto files to compile to in ml 
*)
open Core.Std

module ByteStream : sig
  type t

  val of_string : string -> t

  val read : t -> char option

end = struct
  type t =
    { arr: string;
      mutable i : int
    }

  let of_string s =
    { arr = s;
      i = -1
    }

  let read t =
    try
      t.i <- t.i + 1;
      Some (String.get t.arr t.i)
    with _ -> None
end

exception Decode_Error of string

let to_binary i =  (* returns list of ints representing binary, msb on left *)
  let rec gather i =
    let cur = i mod 2 in
    let rest = i / 2 in
    match rest with
    | 0 -> [cur]
    | r -> cur::(gather r)
  in gather i |> List.rev

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
  let rec left_pad n bin =  (* ayy lmao *)
    if List.length bin < n
    then left_pad n (0::bin)
    else bin
  in
  let rec get_bytes bs =  (*get relevant bits while removing msb *)
    match ByteStream.read bs with
    | Some b -> 
      let b = Char.to_int b in
      if (b land 0b10000000) <> 0
      then (b land 0b01111111)::get_bytes bs
      else [b]
    | None -> raise (Decode_Error "incomplete varint")
  in get_bytes bs |> List.rev |> List.map ~f:(Fn.compose (left_pad 7) to_binary) 
     |> List.concat |> to_dec

let decode bs =
  match ByteStream.read bs with
  | Some b ->
    let b = Char.to_int b in
    let fieldn = b lsr 3 in
    let wire = b land 0b111 in
    begin
      match wire with
      | 0 -> (fieldn, decode_varint bs)
      | _ -> raise (Decode_Error "invalid wire type")
    end
  | None -> raise (Decode_Error "nothing left")

module rec A : sig
  type t =
    { a: int;
    }

  val a : t -> int

  val encode : t -> string

  val decode : string -> t

end = struct

  type t =
    { a: int;
    } 

  let a t = t.a

  let encode t =
    let encode_a = encode_varint 1 t.a in
    encode_a

  let decode s =
    let bs = ByteStream.of_string s in
    match decode bs with
    | (1, v) -> { a = v }
    | _ -> raise (Decode_Error "invalid tag")

end
