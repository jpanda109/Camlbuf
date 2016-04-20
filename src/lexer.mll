{
open Lexing
open Parser
exception SyntaxError of string
}

rule read =
  parse
  | [' ' '\t']+ { read lexbuf }
  | '\r' | '\n' | "\r\n" { new_line lexbuf; read lexbuf }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "<" { LABRACK }
  | ">" { RABRACK }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "=" { ASSIGN }
  | "//" ['a'-'z' 'A'-'Z' '-' '_']* { read lexbuf }
  | "message" { MESSAGE }
  | "service" { SERVICE }
  | "syntax" { SYNTAX }
  | "reserved" { RESERVED }
  | "enum" { ENUM }
  | "singular" { SINGULAR }
  | "repeated" { REPEATED }
  | "option" { OPTION }
  | "import" { IMPORT }
  | "package" { PACKAGE }
  | "public" { PUBLIC }
  | "oneof" { ONEOF }
  | "map" { MAP }
  | ['a'-'z' 'A'-'Z']+ { ID (Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and read_string buf =
  parse
  | '"' { STR (Buffer.contents buf) }
  | '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
