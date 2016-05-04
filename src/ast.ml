open Core.Std

module rec Field : sig
  type rule =
    | Singular
    | Repeated

  type t =
    { rule: rule;
      ftype: FieldType.t;
      name: string;
      tag: int
    }
end = Field

and FieldType : sig

  type t =
    | Int32   | Int64   | UInt32   | UInt64   | SInt32 | SInt64
    | Fixed32 | Fixed64 | SFixed32 | SFixed64 | Bool   | Bytes
    | String  | Double | Float
    | Custom of string

  val of_string : string -> t

  val to_string : t -> string

end = struct

  type t =
    | Int32   | Int64   | UInt32   | UInt64   | SInt32 | SInt64
    | Fixed32 | Fixed64 | SFixed32 | SFixed64 | Bool   | Bytes
    | String  | Double | Float
    | Custom of string

  let of_string = function
    | "int32" -> Int32
    | "int64" -> Int64
    | "uint32" -> UInt32
    | "uint64" -> UInt64
    | "sint32" -> SInt32
    | "sint64" -> SInt64
    | "fixed32" -> Fixed32
    | "fixed64" -> Fixed64
    | "sfixed32" -> SFixed32
    | "sfixed64" -> SFixed64
    | "bool" -> Bool
    | "bytes" -> Bytes
    | "string" -> String
    | "double" -> Double
    | "float" -> Float
    | s -> Custom s

  let to_string = function
    | Int32 -> "int32"
    | Int64 -> "int64"
    | UInt32 -> "uint32"
    | UInt64 -> "uint64"
    | SInt32 -> "sint32"
    | SInt64 -> "sint64"
    | Fixed32 -> "fixed32"
    | Fixed64 -> "fixed64"
    | SFixed32 -> "sfixed32"
    | SFixed64 -> "sfixed64"
    | Bool -> "bool"
    | Bytes -> "bytes"
    | String -> "string"
    | Double -> "double"
    | Float -> "float"
    | Custom s -> s

end

and Message : sig
  type t =
    { name: string;
      fielddecs: Field.t list;
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

