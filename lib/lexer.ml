open Core

type t = 
    { input : string
    ; position : int
    ; ch: char option
    } [@@deriving show]

let init input = 
    if String.is_empty input then
        {input; position = 0; ch = None}
    else
        {input; position = 0; ch = Some (String.get input 0)}

let prev_ch lexer =
    String.get lexer.input (lexer.position -1)

let get s i = 
    if i >= String.length s then
        None
    else
        Some (String.get s i)

(**Will only check after new line, not in the middle*)
let is_indent lexer ch =
    Char.is_whitespace ch && (Int.equal lexer.position 0 || Char.equal (prev_ch lexer) '\n')

(**Checks if there is no non whitespace char last newline and current pos*)
let is_first_in_line lexer = 
    let rec loop input i =
        if i >= 0 then
            let ch = String.get input i in
            if Char.equal '\n' ch then
                true
            else if Char.is_whitespace (String.get input i) then
                loop input (i-1)
            else
                false
        else
            false
    in
    loop lexer.input (lexer.position -1)

let can_be_header lexer =
    Int.equal lexer.position 0 || Char.equal (prev_ch lexer) '\n'

let rec next_token lexer = 
    let open Token in
    match lexer.ch with
    | None -> lexer, None
    | Some ch ->
        (* let () = printf "%c..\n" ch in *)
        match ch with
        | '\n' -> advance lexer, Some (NewLine)
        | '*' ->
            let start = lexer.position in
            let lexer, i = parse_until lexer (fun ch ->
                match ch with
                | None -> false
                | Some ch -> Char.equal '*' ch) in
            lexer, Some (Star (i - start))
        | '-' when is_list lexer -> advance (advance lexer), Some (List)
        | c when is_indent lexer c -> 
            let lexer, count = skip_whitespace lexer in
            lexer, Some (Indent count)
        | '#' when can_be_header lexer -> is_header lexer
        | '`' -> is_code lexer
        | _ -> parse_text lexer
(* lexer, Some (Illegal) *)


(** Moves lexer one position further if there is more left of input
    @param lexer
    @return new lexer*)
and advance lexer = 
    if lexer.position >= String.length lexer.input - 1 then
        {lexer with position = lexer.position + 1; ch = None}
    else
        let position = lexer.position + 1 in
        {lexer with position; ch = Some (String.get lexer.input position)}

(** Skips all whitespace
    @return new lexer, amount of space skipped
    *)
and skip_whitespace lexer =
    let line = lexer.input in
    let rec count i c =
        if String.is_empty line then
            i, c
        else if Char.is_whitespace (String.get line i) then
            let ch = String.get line i in
            let add = match ch with
                | ' ' -> 1
                | '\t' -> 4
                | _ -> 1
            in
            count (i+1) (c+add)
        else
            i, c
    in
    let i, c = count lexer.position 0 in
    { lexer with position = i; ch = (get line i) }, c

and peek lexer =
    get lexer.input (lexer.position+1)

(** Parses to see if it is header, if not parse as text instead *)
and is_header lexer =
    let line = lexer.input in
    let rec count i =
        if String.is_empty line || i >= String.length line then
            i
        else if Char.equal '#' (String.get line i) then
            count (i+1)
        else if i > lexer.position && Char.equal (String.get line i) ' ' then
            i
        else
            0
    in
    let i = count lexer.position in
    if Int.equal i 0 then
        (*Parse text instead*)
        parse_text (advance lexer)
    else
        let i = i+1 in
        { lexer with position = i; ch = (get line i) }, Some (Header (i - 1 - lexer.position))

and parse_text lexer =
    let lexer, str = read_while lexer (fun ch -> not (Token.is_inline_identifier ch || Char.equal ch '\n')) in
    if String.is_empty str then
        lexer, None
    else
        lexer, Some (Text str)

and is_code lexer =
    let rec loop n = 
        if n >= 3 then true else
        if lexer.position + n >= (String.length lexer.input) then
            false
        else
            let ch = String.get lexer.input (lexer.position + n) in
            match ch with
            | '`' -> loop (n+1)
            | _ -> false
    in
    if loop 3 then
        {lexer with position = lexer.position + 3; ch = get lexer.input (lexer.position + 3)}, Some (Code)
    else 
        lexer, None

and read_while lexer condition =
    let start = lexer.position in
    let lexer, endpos = parse_until lexer (fun ch ->
        match ch with
        | None -> false
        | Some ch -> 
            (* let () = printf "'%c' - %b\n" ch (condition ch) in *)
            condition ch
    ) in
    lexer, String.slice lexer.input start endpos

and parse_until lexer condition =
    let rec loop lexer = if condition lexer.ch then loop @@ advance lexer else lexer in
    let lexer = loop lexer in
    lexer, lexer.position

and is_list lexer =
    is_first_in_line lexer &&
    (match peek lexer with
        | None -> false
        | Some ch -> 
            match ch with 
            | ' ' -> true
            | _ -> false)




(* let toString lexer = Fmt. *)
let pp = pp ;;
let show = show ;;
