let () = print_endline "Hello, World!"

let lexer = HTcaML2.Lexer.init "# *Test* \n ## Hei Paa deg\n\nTesting\n- Liste\n\t- liste```lua\nlocal l = 1```";;

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
            | Star      -> Fmt.pr "Star\n"
            | Illegal   -> Fmt.pr "Illegal\n"
        in
        (* let () = Fmt.pr "%s\n" (HTcaML2.Lexer.show lexer) in *)
        main lexer
;;
main lexer;;
