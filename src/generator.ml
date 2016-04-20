open Core.Std
open Ast

let make_typedec {TypeDec.rule = r; ftype = ft; name = n; _ } =
  let oct =
    match ft with
    | "int32"  | "int64"   | "uint32"  | "uint64"   | "sint32" 
    | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" -> "int"
    | t -> t
  in
  let optional = match r with
    | TypeDec.Singular -> "option"
    | TypeDec.Repeated -> "list" in
  Printf.sprintf "%s: %s %s" n oct optional

let make_message {Message.typedecs = tds; Message.name = name} =
  let msg_contents = List.map ~f:make_typedec tds 
                     |> String.concat ~sep:";\n    " in
  Printf.sprintf "type %s =\n  { %s\n  }" name msg_contents

let make_protofile {Protofile.messages = msgs} =
  List.map ~f:make_message msgs |> String.concat ~sep:"\n\n"
