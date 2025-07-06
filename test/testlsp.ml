open Why3_lsp.Rpc

(*Idea: have a read_line loop that appends data to a list, which fulfills a promise to start the processing*)

let rec server () = 
  let input = read_line () in
    interp input; server ();;

let () = server ();

(*
{"version": "2.0", "num": 1}
{"version": "2.0", "num": 5}
{"version": "2.0", "num": 100}
{"version": "2.0", "num": 9999}
{"version": "2.0", "num": 0}
{"version": "2.0", "num": -42}*)
