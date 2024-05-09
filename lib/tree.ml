open Core

type builder =
    { nodes: Token.t list
    ; position: int
    ; t: Token.t option
    ; indent: int}

let rec init_builder input =
    let lexer = Lexer.init input in
    let tokens = get_tokens lexer in
    let t = List.nth tokens 0 in
    { nodes = tokens; position = 0; t; indent = 0}

and advance builder = 
    (* Adds indent if advancing past indent *)
    let indent = match builder.t with
        | Some (Indent i) ->  i 
        | Some NewLine -> 0
        | _ -> builder.indent in
    let position = builder.position + 1 in
    { builder with position; t = List.nth builder.nodes position; indent}

(** @return next builder and node*)
and next_node builder = 
    match builder.t with
    | None -> builder, None
    | Some t ->
        match t with
        | Illegal -> builder, None
        | Indent _ -> advance builder, Some Node.Next
        | Header h -> header_node builder h
        (* | Header h -> advance builder, Some Next *)
        | List -> 
            let builder, body = parse_list builder in
            builder, Some (ListBody body)
        | Code -> advance builder, Some Next
        | NewLine -> advance builder, Some Next
        | _ -> parse_paragraph builder

(** recursivly gets inline from a builder
    @param newline wether to continue on newline token
    @return next builder, list of items*)
and get_inline builder ~newline =
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
    match builder.t with
    | Some NewLine -> advance builder, List.rev items
    | _ -> builder, List.rev items

and parse_paragraph builder = 
    let indent = builder.indent in
    let builder, items = get_inline builder ~newline:true in
    builder, Some (Node.Paragraph {items = items; indent = indent})

and header_node builder number =
    let builder, items = get_inline builder ~newline:false in
    let header = match number with
        | 1 -> Node.Header1 items
        | 2 -> Node.Header2 items
        | 3 -> Node.Header3 items
        | 4 -> Node.Header4 items
        | 5 -> Node.Header5 items
        | 6 -> Node.Header6 items
        | _ -> Node.Header1 items
    in
    builder, Some (header)

and parse_list builder =
    let open Node in
    let rec get_item builder items = 
        match builder.t with
        | Some List -> 
            let indent = builder.indent in
            let builder, inline = get_inline (advance builder) ~newline:false in

            let haschild =
                match builder.t with
                | Some (Indent i) -> (i > indent)
                | _ -> false in

            let builder, inner =
                if haschild then
                    let builder = advance builder in
                    match builder.t with
                    | Some List ->
                        let builder, body = parse_list builder in
                        builder, Some body
                    | _ -> builder, None
                else
                    builder, None in
            let item = {items = inline; inner} in
            (
                (* Only get next list if is on same indent level
                   Higher indented lists are taken care of*)
                match builder.t with
                | Some List when indent = builder.indent -> get_item builder (item::items)
                | Some Indent i when indent = i -> get_item (advance builder) (item::items)
                | _ -> builder, List.rev (item::items)
            )
        | _ -> builder, List.rev items
    in
    let indent = builder.indent in
    let builder, children = get_item builder [] in
    builder, {children; indent}

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
