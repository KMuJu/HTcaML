    open Core

let () = print_endline "Hello, World!"

let lexer = HTcaML.Lexer.init "# *Test* \n## Hei Paa deg\n\nTesting\n- Liste\n\t- liste```lua\nlocal l = 1```";;

Fmt.pr "\n%s\n" (HTcaML.Lexer.show lexer)

let rec main lexer =
    let lexer, t = HTcaML.Lexer.next_token lexer in

    match t with
    | None -> Fmt.pr "None :/ \n"
    | Some token ->
        let () = 
            match token with
            | Header s  -> Fmt.pr "Header: %d\n" s
            | Text s    -> Fmt.pr "Text: '%s'\n" s
            | NewLine   -> Fmt.pr "\tNewline \n"
            | Indent i  -> Fmt.pr "Indent: %d\n" i
            | List      -> Fmt.pr "List\n" 
            | Code      -> Fmt.pr "Code\n"
            | Star i    -> Fmt.pr "Star: %d\n" i
            | Illegal   -> Fmt.pr "Illegal\n"
        in
        (* let () = Fmt.pr "%s\n" (HTcaML.Lexer.show lexer) in *)
        main lexer
;;
main lexer;;

let get_tokens builder =
    let rec loop builder l = 
        let next = HTcaML.Tree.next_node builder in
        match next with
        | _, None -> List.rev l
        | lexer, Some token ->
            loop lexer (token :: l)
    in
    loop builder []

let tree nodes =
    List.iter nodes ~f:(fun n ->
        match n with
        | HTcaML.Node.Next -> Fmt.pr "Next\n"
        | Header1 h -> Fmt.pr "Header1: %s\n" (HTcaML.Node.show_paragraph h)
        | Header2 h -> Fmt.pr "Header2: %s\n" (HTcaML.Node.show_paragraph h)
        | Header3 h -> Fmt.pr "Header3: %s\n" (HTcaML.Node.show_paragraph h)
        | Header4 h -> Fmt.pr "Header4: %s\n" (HTcaML.Node.show_paragraph h)
        | Header5 h -> Fmt.pr "Header5: %s\n" (HTcaML.Node.show_paragraph h)
        | Header6 h -> Fmt.pr "Header6: %s\n" (HTcaML.Node.show_paragraph h)
        | Paragraph p -> Fmt.pr "Paragraph: %s\n" (HTcaML.Node.show_pbody p)
        | ListBody b -> Fmt.pr "List: %s\n" (HTcaML.Node.show_list b)
        | _ -> Fmt.pr "IDK\n")
;;

let builder = HTcaML.Tree.init_builder
"Tekst
**bold** *italic*

teskt ti
# Header
- Test
    - TT
- AA
    - uu
    - LKJ
        - ...
    - laksj
        - *09*

## Header2

### Header3
- Kaer
    - Hai" in
tree (get_tokens builder);;
