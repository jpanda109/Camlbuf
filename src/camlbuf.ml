open Core.Std
open Lexing

open Lexer

let print_position outc lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outc "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Some (Parser.prog Lexer.read lexbuf) with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    None

let get_ast filename =
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  begin match parse_with_error lexbuf with
  | Some ast -> ast
  | None -> raise (Failure "couldn't parse file")
  end

let print_tokens filename =
  let contents = In_channel.read_all filename in
  let lexbuf = Lexing.from_string contents in
  let rec f lexbuf = match Lexer.read lexbuf with
    | Parser.EOF as token -> [token]
    | token -> token::(f lexbuf) in
  f lexbuf 
  |> List.map ~f:Proto_debug.string_of_token 
  |> String.concat ~sep:", " 
  |> print_endline

let print_ast ast =
  Proto_debug.proto_to_string ast |> print_endline

let compile_ast ast =
  Generator.make_protofile ast

let command =
  Command.basic
    ~summary:"evaluate an edl language file"
    Command.Spec.(
      empty
        +> flag "-d" no_arg ~doc:"Debug"
        +> anon ("filename" %: file)
    )
    (fun debug filename () -> 
       let ast = get_ast filename in
       begin
         match debug with
         | true -> print_tokens filename; Proto_debug.proto_to_string ast |> print_endline
         | false -> ()
       end; Generator.make_protofile ast |> print_endline
    )

let () =
  Command.run command
