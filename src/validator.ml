open Core.Std
open Ast

let validate { Protofile.messages = msgs } =
  List.map ~f:(fun m -> m.Message.name) msgs |> List.contains_dup |> not

