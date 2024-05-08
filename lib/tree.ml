open Core
type t = 
    { nodes: Node.t list}

type builder =
    { nodes: Token.t list
    ; position: int
    ; t: Token.t option
    ; indent: int
    ; len: int}

let rec init_builder input =
    let open Lexer in
    let lexer = Lexer.init input in
    let tokens = get_tokens lexer in
    let t = List.nth tokens 0 in
    { nodes = tokens; position = 0; t; indent = 0; len = List.length tokens}

and advance builder = 
    let position = builder.position + 1 in
    { builder with position; t = List.nth builder.nodes position }

and next_node builder = 
    match builder.t with
    | None -> builder, None
    | Some t ->
        match t with
        | Illegal -> builder, None
        | Indent i -> { (advance builder) with indent = i }, Some Node.Next
        | Header h -> header_node builder h
        (* | Header h -> advance builder, Some Next *)
        | List -> advance builder, Some Next
        | Code -> advance builder, Some Next
        | NewLine -> advance builder, Some Next
        | _ -> parse_paragraph builder

(** recursivly gets inline from a builder
    @param newline wether to continue on newline token
    @return next builder, list of items*)
and get_inline builder newline =
    let rec loop builder items =
        let builder, items = match builder.t with
            | None -> builder, items
            | Some t ->
                match t with
                | Text s -> 
                    loop (advance builder) ((Node.Text s)::items)
                | Star 1 -> 
                    (
                        match List.nth builder.nodes (builder.position + 1) with
                        | Some (Text s) -> loop (advance (advance builder)) ((Italic s)::items)
                        | _ -> loop (advance builder) (items)
                    )
                | Star 2 -> 
                    (
                        match List.nth builder.nodes (builder.position + 1) with
                        | Some (Text s) -> loop (advance (advance builder)) ((Bold s)::items)
                        | _ -> loop (advance builder) (items)
                    )
                | Header _ -> 
                    if Int.equal (List.length items) 0 then
                        loop (advance builder) items
                    else
                        builder, items
                | _ -> builder, items
        in
        if newline then
            (* If next token is NewLine and is not double NewLine, will loop further*)
            match builder.t with
            | None -> builder, items
            (* | _ -> builder, items *)
            | Some t -> 
                match t with
                | NewLine -> 
                    (
                        match (peek builder) with
                        | Some Token.NewLine -> builder, items
                        | _ -> 
                            let builder, items = loop (advance builder) (NewLine::items) in
                            builder, items
                    )
                | _ -> builder, items
        else builder, items
    in
    let builder, items = loop builder [] in
    builder, List.rev items

and parse_paragraph builder = 
    let indent = builder.indent in
    let builder, items = get_inline builder true in
    builder, Some (Node.Paragraph {items = items; indent = indent})

and header_node builder number =
    let () = printf "header %d\n" number in
    let builder, items = get_inline builder false in
    let header = match number with
        | 1 -> Node.Header1 items
        | 2 -> Node.Header2 items
        | 3 -> Node.Header3 items
        | 4 -> Node.Header4 items
        | 5 -> Node.Header5 items
        | 6 -> Node.Header6 items
        | _ -> Node.Header1 items
    in
    {builder with indent = 0}, Some (header)

and get_tokens lexer =
    let rec loop lexer l = 
        let next = Lexer.next_token lexer in
        match next with
        | _, None -> List.rev l
        | lexer, Some token ->
            loop lexer (token :: l)
    in
    loop lexer []

and peek builder =
    List.nth builder.nodes (builder.position+1)
