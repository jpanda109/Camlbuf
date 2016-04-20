open Core.Std
open Ast

let make_typedec {TypeDec.rule = r; ftype = ft; name = n; _ } =
  let oct =
    match ft with
    | "int32"  | "int64"   | "uint32"  | "uint64"   | "sint32" 
    | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64" -> "int"
    | "string" -> "string"
    | "bool" -> "bool"
    | "bytes" -> "string"
    | "double" | "float" -> "float"
    | t -> t
  in
  let optional = match r with
    | TypeDec.Singular -> "option"
    | TypeDec.Repeated -> "list" in
  Printf.sprintf "%s: %s %s" n oct optional

let make_message {Message.typedecs = tds; Message.name = name} =
  let msg_contents = List.map ~f:make_typedec tds 
                     |> List.map ~f:(fun s -> String.concat ["      ";s;";"])
                     |> String.concat ~sep:"\n" in
  let lines = [
    Printf.sprintf "module rec %s : sig" name;
    Printf.sprintf "  type t =";
    "    {";
    msg_contents;
    Printf.sprintf "    }";
    Printf.sprintf "end = %s" name
  ] in String.concat ~sep:"\n" lines

let make_protofile {Protofile.messages = msgs} =
  String.concat [
    "open Core.Std\n\n";
    List.map ~f:make_message msgs |> String.concat ~sep:"\n\n"
  ]
