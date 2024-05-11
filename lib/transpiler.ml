open Core

let init input =
    let rec get builder nodes =
        let builder, token = Tree.next_node builder in
        match token with
        | None -> nodes
        | Some t -> get builder (t::nodes)
    in
    let builder = Tree.init_builder input in
    let nodes = get builder [] in
    nodes 

let rec transpile input = 
    let rec loop nodes s =
        match nodes with
        | [] -> s
        | n::l -> loop l s^(tostring n)
    in
    let ast = init input in
    loop ast ""

and tostring node =
    match node with
    | Node.Header1 h -> "<h1>"^paragraph h^"</h1>\n"
    | Header2 h -> "<h2>"^paragraph h^"</h2>\n"
    | Header3 h -> "<h3>"^paragraph h^"</h3>\n"
    | Header4 h -> "<h4>"^paragraph h^"</h4>\n"
    | Header5 h -> "<h5>"^paragraph h^"</h5>\n"
    | Header6 h -> "<h6>"^paragraph h^"</h6>\n"
    | ListBody l -> list_body l
    | Paragraph p -> pbody p
    | Next -> ""
    | _ -> "IDK\n"

and list_body lb =
    let rec loop children s =
        match children with
        | [] -> s
        | c::l -> loop l ((list_item c)^s)
    in
    let inner = loop lb.children "" in
    (* "<ul style='margin-left:"^(string_of_int lb.indent)^"em'>\n"^inner^"</ul>\n" *)
    "<ul>\n"^inner^"</ul>\n"

and list_item list_item =
    let inner =
        match list_item.inner with
        | None -> ""
        | Some body -> "\n"^list_body body^"\n"
    in
    "<li>\n"^(paragraph list_item.items)^inner^"</li>\n"

and pbody p =
    paragraph p.items

and paragraph p =
    let rec loop l s =
        match l with
        | [] -> s
        | n::l -> loop l (s^(inline n))
    in
    "<p>"^(loop p "")^"</p>"

and inline node =
    match node with
    | Node.Text s   -> s
    | Bold s        -> "<b>"^ s ^"</b>"
    | Italic s      -> "<i>"^ s ^"</i>"
    | NewLine       -> "\n<br>\n"
