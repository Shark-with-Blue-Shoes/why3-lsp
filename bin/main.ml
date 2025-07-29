open Format
open Printf

open Why3_lsp.Server
open Why3_lsp.Scheduler
open Why3_lsp.Runtime

let () =
  let fmt = std_formatter in
  printf "LSP Server (simplified) starting. Type messages and press Enter. Ctrl+D to exit.\n%!";
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of notifications, applying treat_notification, returns true*) 
    (fun () -> List.iter
        (fun n -> treat_notification fmt n) (get_notified ()); 
        true);
  Unix_scheduler.main_loop (interp fmt);;

