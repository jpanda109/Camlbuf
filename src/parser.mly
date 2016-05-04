%{
open Core.Std
open Ast

module Declaration = struct
  type t =
    | Message of Message.t
    | Enum of Enum.t
    | Field of FieldDec.t
end
%}
%token LBRACE RBRACE LBRACK RBRACK LABRACK RABRACK SEMICOLON COMMA ASSIGN EOF
%token MESSAGE SERVICE SYNTAX RESERVED 
ENUM SINGULAR REPEATED OPTION IMPORT PACKAGE PUBLIC ONEOF MAP
%token <int> NUM
%token <string> ID
%token <string> STR

%start <Ast.Protofile.t> prog

%%
prog:
  messages = list(message); EOF;
    { { messages = messages } };

declaration:
  | msg = message { Declaration.Message msg }
  | enum = enum { Declaration.Enum enum }
  | fielddec = fielddec { Declaration.Field fielddec }
  ;

message:
  MESSAGE; name = ID; LBRACE; decs = list(declaration); RBRACE;
    { 
      let update_message msg = function
        | Declaration.Message m -> Message.({ msg with messages = m::(msg.messages) })
        | Declaration.Field f -> { msg with fielddecs = f::(msg.fielddecs) }
        | Declaration.Enum e -> { msg with enums = e::(msg.enums) }
      in
      List.fold 
        ~f:update_message 
        ~init:Message.({name = name; fielddecs = []; messages = []; enums = []})
        decs
    };

fielddec:
  rule_opt = option(rule); ftype = ID; name = ID; ASSIGN; tag = NUM; SEMICOLON;
    { let rule = match rule_opt with
                 | None -> FieldDec.Singular
                 | Some r -> r
      in FieldDec.({rule = rule; ftype = ftype; name = name; tag = tag})
    };

enum:
  ENUM; name = ID; LBRACE; vals = list(enum_val); RBRACE;
    { Enum.({name = name; vals = String.Map.of_alist_exn vals}) }

enum_val:
  name = ID; ASSIGN; tag = NUM; SEMICOLON { (name, tag) };

rule:
  | SINGULAR { FieldDec.Singular }
  | REPEATED { FieldDec.Repeated }
  ;
