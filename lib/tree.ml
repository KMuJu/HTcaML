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
    { builder with position; t = List.nth builder.nodes position}

and next_node builder = 
    match builder.t with
    | None -> builder, None
    | Some t ->
        match t with
        | _ -> advance builder, None

(* and header_node builder number indent =
    let rec loop tokens nodes = 
        match tokens with
        | [] -> List.rev nodes
        | t::l ->
            []
    in
    let tokens = get_tokens_to_newline builder in
    builder, None *)

and get_tokens lexer =
    let rec loop lexer l = 
        let next = Lexer.next_token lexer in
        match next with
        | _, None -> List.rev l
        | lexer, Some token ->
            loop lexer (token :: l)
    in
    loop lexer []

and get_tokens_to_newline builder =
    let builder, tokens = read_while builder (fun t ->
        match t with
        | Token.NewLine -> false
        | _ -> true
    ) in
    (* Advance past the newline token *)
    advance builder, tokens

and read_while builder condition =
    let start = builder.position in
    let builder, endpos = parse_until builder (fun t ->
        match t with
        | None -> false
        | Some t -> 
            (* let () = printf "'%c' - %b\n" ch (condition ch) in *)
            condition t
    ) in
    builder, List.sub builder.nodes ~pos:start ~len:(endpos-start)

and parse_until builder condition =
    let rec loop builder = 
        if condition builder.t then
            loop @@ advance builder 
        else
            builder
        in
    let builder = loop builder in
    builder, builder.position
