let read_lines file =
  In_channel.with_open_text file In_channel.input_all
in


if Array.length Sys.argv < 2 then
    print_endline "Need to specify file"
else

    let file = read_lines Sys.argv.(1) in 

    let () = print_endline (HTcaML.Transpiler.transpile file) in
    ()
