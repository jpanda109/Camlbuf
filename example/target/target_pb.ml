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

let to_binary i =  (* returns list of ints representing binary, lsb on left *)
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
  let rec get_chunks n =
    if n >= 128 
    then (n mod 128) + 128 :: get_chunks (n / 128)
    else [n]
  in key::get_chunks i |> List.map ~f:Char.of_int_exn |> String.of_char_list

let decode_varint bs =
  let rec get_bytes bs =
    match ByteStream.read bs with
    | Some b ->
      let b = Char.to_int b in
      if b >= 128
      then b mod 128 :: get_bytes bs
      else [b]
    | None -> raise (Decode_Error "incomplete varint")
  in 
  get_bytes bs
  |> List.foldi ~f:(fun i acc b -> acc + b lsl (i * 7)) ~init:0

let encode_string tag s =
  let key = (tag lsl 3) lor 2 in
  let l = String.length s in
  String.concat [Char.to_string (Char.of_int_exn key); Char.to_string (Char.of_int_exn l); s]

let decode_string bs =
  let l = 
    match ByteStream.read bs with
    | Some b -> Char.to_int b
    | None -> raise (Decode_Error "need string length")
  in
  let rec get_bytes n =
    if n > 0
    then
      match ByteStream.read bs with
      | Some b -> b::get_bytes (n-1)
      | None -> raise (Decode_Error "string length doesn't match number of bytes")
    else []
  in String.of_char_list (get_bytes l)

let decode_uit32 = decode_varint

let decode_uint64 = decode_varint

let get_meta bs =
  match ByteStream.read bs with
  | Some b ->
    let b = Char.to_int b in
    let tag = b lsr 3 in
    let wire = b land 0b111 in
    Some (tag, wire)
  | None -> None

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

  module Enum : sig

    type t =
      | A
      | B

    val to_val : t -> int

    val of_val : int -> t

    val to_string : t -> string

  end

  type t =
    { a: int;
      b: string;
      c: Enum.t;
    }

  val a : t -> int

  val b : t -> string

  val c : t -> Enum.t

  val encode : t -> string

  val decode : string -> t

end = struct

  module Enum = struct
    type t =
      | A
      | B

    let to_val = function
      | A -> 0
      | B -> 1

    let of_val = function
      | 0 -> A
      | 1 -> B
      | _ -> A

    let to_string = function
      | A -> "A"
      | B -> "B"
  end

  type t =
    { a: int;
      b: string;
      c: Enum.t;
    } 

  let a t = t.a

  let b t = t.b

  let c t = t.c

  let encode t =
    let encode_a = encode_varint 1 t.a in
    let encode_b = encode_string 2 t.b in
    let encode_c = encode_varint 3 (Enum.to_val t.c) in
    String.concat [encode_a; encode_b; encode_c]

  let decode s =
    let bs = ByteStream.of_string s in
    let rec make_t t =
      match get_meta bs with
      | Some (1, 0) -> make_t { t with a = decode_uint64 bs }
      | Some (2, 2) -> make_t { t with b = decode_string bs }
      | Some (3, 0) -> make_t { t with c = Enum.of_val (decode_uint64 bs) }
      | None -> t
      | _ -> raise (Decode_Error "invalid tag")
    in make_t { a = 0; b = ""; c = Enum.A }

end

and B : sig

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
    String.concat [encode_a]

  let decode s =
    let bs = ByteStream.of_string s in
    let rec make_t t =
      match get_meta bs with
      | Some (1, 0) -> make_t { a = decode_uint64 bs }
      | _ -> raise (Decode_Error "invalid tag")
    in make_t { a = 0 }

end
