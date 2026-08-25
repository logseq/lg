type t =
  | Byte_string of string
  | Integer of int64
  | List of t list
  | Dictionary of (string * t) list

val max_byte_string_bytes : int
val max_nesting_depth : int
val max_value_bytes : int

val to_string : t -> (string, string) result
val of_string : string -> (t, string) result
val write : out_channel -> t -> (unit, string) result
val read : in_channel -> (t option, string) result
