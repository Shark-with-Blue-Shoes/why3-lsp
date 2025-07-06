open Lwt
open Why3_lsp.Rpc

let server () =
  let p = 
    let%lwt input = (Lwt_io.read_line Lwt_io.stdin) in
    Lwt.return (interp input) in
    p

let _ = Lwt_main.run (server ()); 
