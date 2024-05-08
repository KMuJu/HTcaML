type inline = 
    | Text of string
    | Bold of string
    | Italic of string
    | NewLine
[@@deriving show]

type paragraph = inline list
[@@deriving show]

type list_body = 
    { children: list_item list
    ; indent: int}

and list_item = 
    { items: inline list
    ; inner: list_body option}

type code_text = 
    | CodeText of string
    | CodeNewLine

type pbody =
    { items: paragraph
    ; indent: int}
[@@deriving show]

type t = 
    | Next
    | Header1 of paragraph
    | Header2 of paragraph
    | Header3 of paragraph
    | Header4 of paragraph
    | Header5 of paragraph
    | Header6 of paragraph
    | ListBody of list_body
    | Code of 
        { code: string * code_text list
        ; indent: int }
    | Paragraph of pbody

let show_inline = show_inline;;
let pp_inline = pp_inline;;
let show_paragraph = show_paragraph;;
let pp_paragraph = pp_paragraph;;
let show_pbody = show_pbody;;
let pp_pbody = pp_pbody;;
