type inline = 
    | Text of string
    | Bold of string
    | Italic of string

type paragraph = inline list

type item = paragraph

type code_text = 
    | Text of string
    | NewLine

type header = 
    { items: inline list
    ; indent: int }

type t = 
    | Header1 of header
    | Header2 of header
    | Header3 of header
    | Header4 of header
    | Header5 of header
    | Header6 of header
    | ListBody of 
        { items: item list
        ; indent: int }
    | Code of 
        { items: string * code_text list
        ; ident: int }
    | Paragraph of 
        { items: paragraph list
        ; ident: int}

