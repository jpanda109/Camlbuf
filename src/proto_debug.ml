open Core.Std
open Ast

let td_to_string { Field.rule = rule; ftype = ftype; name = name; tag = tag } =
  let rule_string = match rule with
    | Field.Singular -> "singular"
    | Field.Repeated -> "repeated"
  in
  Printf.sprintf "Decl { rule: %s; ftype: %s; name: %s; tag: %d }" rule_string (FieldType.to_string ftype) name tag

let enum_to_string { Enum.name = name; vals = vals } =
  let vals_string = String.Map.keys vals |> String.concat ~sep:", " in
  Printf.sprintf
    "Enum { name: %s; vals: %s }"
    name vals_string

let rec msg_to_string 
    { Message.name = name; 
      fielddecs = tdecs; 
      messages = msgs; 
      enums = enums 
    } =
  let tdecs_string = List.map ~f:td_to_string tdecs |> String.concat ~sep:", " in
  let msgs_string = List.map ~f:msg_to_string msgs |> String.concat ~sep:", " in
  let enums_string = List.map ~f:enum_to_string enums |> String.concat ~sep:", " in
  Printf.sprintf 
    "Message { name: %s; fielddecs: %s; messages: %s; enums: %s }" 
    name tdecs_string msgs_string enums_string

let proto_to_string { Protofile.messages = msgs } =
  let msgs_string = List.map ~f:msg_to_string msgs |> String.concat ~sep:", " in
  Printf.sprintf "Protofile { msgs: %s }" msgs_string

let string_of_token = function
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.COMMA -> "COMMA"
  | Parser.ENUM -> "ENUM"
  | Parser.EOF -> "EOF"
  | Parser.ID s -> Printf.sprintf "ID %s" s
  | Parser.IMPORT -> "IMPORT"
  | Parser.LABRACK -> "LABRACK"
  | Parser.LBRACE -> "LBRACE"
  | Parser.LBRACK -> "LBRACK"
  | Parser.MAP -> "MAP"
  | Parser.MESSAGE -> "MESSAGE"
  | Parser.NUM n -> Printf.sprintf "NUM %d" n
  | Parser.ONEOF -> "ONEOF"
  | Parser.OPTION -> "OPTION"
  | Parser.PACKAGE -> "PACKAGE"
  | Parser.PUBLIC -> "PUBLIC"
  | Parser.RABRACK -> "RABRACK"
  | Parser.RBRACE -> "RBRACE"
  | Parser.RBRACK -> "RBRACK"
  | Parser.REPEATED -> "REPEATED"
  | Parser.RESERVED -> "RESERVED"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.SERVICE -> "SERVICE"
  | Parser.SINGULAR -> "SINGULAR"
  | Parser.STR s -> Printf.sprintf "STR %s" s
  | Parser.SYNTAX -> "SYNTAX"
