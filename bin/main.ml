  open Why3_lsp.Rpc 

let rec server () = 
  let input = read_line () in
    interp input; server ();;

let () = server ();
