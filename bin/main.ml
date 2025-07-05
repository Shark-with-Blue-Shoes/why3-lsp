open Lwt
open Why3_lsp.Rpc

let () =
  let p =  
    let%lwt input = (Lwt_io.read_line Lwt_io.stdin) in
    Lwt.return (interp input) in
    Lwt_main.run p;
