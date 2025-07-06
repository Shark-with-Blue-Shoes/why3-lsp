open Why3_lsp.Rpc

(* Main program entry point *)
let () =
  Printf.printf "LSP Server (simplified) starting. Type messages and press Enter. Ctrl+D to exit.\n%!";

  (* Start the consumer thread *)
  let consumer_thread = Thread.create consumer_thread_func () in

  (* Start the producer (main thread will block here, reading stdin) *)
  server_producer ();

  (* Wait for the consumer thread to finish its work and exit *)
  Thread.join consumer_thread;

  Printf.printf "Server shutdown complete.\n%!"

(*
{"version": "2.0", "num": 1}
{"version": "2.0", "num": 5}
{"version": "2.0", "num": 100}
{"version": "2.0", "num": 9999}
{"version": "2.0", "num": 0}
{"version": "2.0", "num": -42}*)
