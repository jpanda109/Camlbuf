open Core.Std

module rec FieldDec : sig
  type rule =
    | Singular
    | Repeated

  type t =
    { rule: rule;
      ftype: string;
      name: string;
      tag: int
    }
end = FieldDec

and Message : sig
  type t =
    { name: string;
      fielddecs: FieldDec.t list;
      messages: Message.t list;
      enums: Enum.t list;
    }
end = Message

and Enum : sig
  type t =
    { name: string;
      vals: int String.Map.t
    }
end = Enum

and Protofile : sig
  type t =
    { messages: Message.t list 
    } 
end = Protofile
