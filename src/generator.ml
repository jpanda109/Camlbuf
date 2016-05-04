open Core.Std
open Ast

module Writer : sig

  type t

  val to_string : t -> string

  val init : t

  val line : t -> string -> t

  val newline : t -> t

  val indent : t -> t

  val unindent : t -> t

end = struct

  type t = 
    { data : string list;
      level : int;
      size : int;
    }

  let to_string t = String.concat ~sep:"\n" t.data

  let init =
    { data = [];
      level = 0;
      size = 2;
    }

  let line t s = { t with data = s::t.data }

  let newline = (Fn.flip line) ""

  let indent t = { t with level = t.level + 1 }

  let unindent t = { t with level = t.level - 1 }

end

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

let make_message {Message.fielddecs = tds; Message.name = name; _} =
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
  let writer = Writer.init in
  let header = Writer.line writer "open Core.Std" in
  List.fold ~f:(fun wr msg -> Writer.line wr (make_message msg)) ~init:header msgs
  |> Writer.to_string
