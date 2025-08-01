open Printf
open Out_channel
open Yojson.Basic

open Why3_lsp.Scheduler
open Why3_lsp.Runtime

let () =
  printf "LSP Server has begun!\n%!";
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of notifications, applying call_notification, returns true*) 
    (fun () -> List.iter
        (fun n -> call_notification n |> pretty_to_channel stdout) (get_notified ()); 
        true);
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of requests, applying call_request, returns true*) 
    (fun () -> List.iter
        (fun n -> call_request n |> pretty_to_channel stdout) (get_requests ()); 
        true);
  Unix_scheduler.main_loop interp;;

