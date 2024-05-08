    open Core

let () = print_endline "Hello, World!"

let lexer = HTcaML2.Lexer.init "# *Test* \n## Hei Paa deg\n\nTesting\n- Liste\n\t- liste```lua\nlocal l = 1```";;

Fmt.pr "\n%s\n" (HTcaML2.Lexer.show lexer)

let rec main lexer =
    let lexer, t = HTcaML2.Lexer.next_token lexer in

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
        (* let () = Fmt.pr "%s\n" (HTcaML2.Lexer.show lexer) in *)
        main lexer
;;
main lexer;;

let get_tokens builder =
    let rec loop builder l = 
        let next = HTcaML2.Tree.next_node builder in
        match next with
        | _, None -> List.rev l
        | lexer, Some token ->
            loop lexer (token :: l)
    in
    loop builder []

let tree nodes =
    List.iter nodes ~f:(fun n ->
        match n with
        | HTcaML2.Node.Next -> Fmt.pr "Next\n"
        | Header1 h -> Fmt.pr "Header: %s\n" (HTcaML2.Node.show_paragraph h)
        | Paragraph p -> Fmt.pr "Paragraph: %s\n" (HTcaML2.Node.show_pbody p)
        | ListBody b -> Fmt.pr "List: %s\n" (HTcaML2.Node.show_list b)
        | _ -> Fmt.pr "IDK\n")
;;

let builder = HTcaML2.Tree.init_builder
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
" in
tree (get_tokens builder);;
