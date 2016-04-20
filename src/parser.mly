%{
open Ast
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

message:
  MESSAGE; name = ID; LBRACE; typedecs = list(typedec); RBRACE;
    { { name = name; typedecs = typedecs } };

typedec:
  rule_opt = option(rule); ftype = ID; name = ID; ASSIGN; tag = NUM; SEMICOLON;
    { let rule = match rule_opt with
                 | None -> TypeDec.Singular
                 | Some r -> r
      in TypeDec.({rule = rule; ftype = ftype; name = name; tag = tag})
    };

rule:
  | SINGULAR { TypeDec.Singular }
  | REPEATED { TypeDec.Repeated }
  ;
