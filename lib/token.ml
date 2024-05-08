type t =
    | Illegal
    | Indent of int
    | NewLine
    | Header of int
    | Text of string
    | Star of int
    | List
    | Code 
[@@deriving show]

let is_inline_identifier ch =
    match ch with
    | '*' -> true
    | '_' -> true
    | '`' -> true
    |  _  -> false

let show = show;;
let pp = pp;;
