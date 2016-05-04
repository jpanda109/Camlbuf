open Core.Std
open Ast

let make_typedec {Field.rule = r; ftype = ft; name = n; _ } =
  let oct =
    FieldType.(
      match ft with
      | Int32  | Int64   | UInt32  | UInt64   | SInt32 
      | SInt64 | Fixed32 | Fixed64 | SFixed32 | SFixed64 -> "int"
      | String -> "string"
      | Bool -> "bool"
      | Bytes -> "string"
      | Double | Float -> "float"
      | Custom t -> t
    )
  in
  let optional = match r with
    | Field.Singular -> "option"
    | Field.Repeated -> "list" in
  Printf.sprintf "%s: %s %s" n oct optional

let make_message {Message.fielddecs = tds; Message.name = name} =
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
