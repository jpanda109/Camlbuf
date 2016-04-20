open Core.Std
open Ast

let td_to_string { TypeDec.rule = rule; ftype = ftype; name = name; tag = tag } =
  let rule_string = match rule with
    | TypeDec.Singular -> "singular"
    | TypeDec.Repeated -> "repeated"
  in
  Printf.sprintf "Decl { rule: %s; ftype: %s; name: %s; tag: %d }" rule_string ftype name tag

let msg_to_string { Message.name = name; typedecs = tdecs } =
  let tdecs_string = List.map ~f:td_to_string tdecs |> String.concat ~sep:", " in
  Printf.sprintf "Message { name: %s; typedecs: %s }" name tdecs_string

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
