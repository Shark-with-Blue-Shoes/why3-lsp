open Why3_lsp.Rpc

(*Idea: have a read_line loop that appends data to a list, which fulfills a promise to start the processing*)

let cmds = ref ["{ \"version\": \"2.0\", \"num\": 3 }"]

let rec in_loop () = 
  cmds := read_line () :: !cmds;
  in_loop ();;

let server () =
  let p = 
    let%lwt input = (Lwt.return (List.hd !cmds)) in
    Lwt.return (interp input) in
  Lwt_main.run p;;

let () =  server (); in_loop ();

(*
{"version": "2.0", "num": 1}
{"version": "2.0", "num": 5}
{"version": "2.0", "num": 100}
{"version": "2.0", "num": 9999}
{"version": "2.0", "num": 0}
{"version": "2.0", "num": -42}*)
