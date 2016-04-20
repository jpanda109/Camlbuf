open Core.Std

module rec TypeDec : sig
  type rule =
    | Singular
    | Repeated

  type t =
    { rule: rule;
      ftype: string;
      name: string;
      tag: int
    }
end = TypeDec

and Message : sig
  type t =
    { name: string;
      typedecs: TypeDec.t list
    }
end = Message

and Protofile : sig
  type t =
    { messages: Message.t list 
    } 
end = Protofile
