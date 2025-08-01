open Format
open Printf
open Out_channel
open Yojson.Basic

open Why3_lsp.Scheduler
open Why3_lsp.Runtime
open Why3_lsp.Server.Lsp_Protocol

let () =
  if is_buffered stdout then
    printf "it is buffered!\n"
  else printf "it is not buffered!\n";
  let fmt = std_formatter in
  printf "LSP Server has begun!\n%!";
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of notifications, applying treat_notification, returns true*) 
    (fun () -> List.iter
        (fun n -> call_notification n |> pretty_to_channel stdout) (get_notified ()); 
        true);
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of requests, applying treat_notification, returns true*) 
    (fun () -> List.iter
        (fun n -> call_request n |> pretty_to_channel stdout) (get_requests ()); 
        true);
  Unix_scheduler.main_loop (interp fmt);;

