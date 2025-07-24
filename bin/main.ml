open Format
open Why3

open Why3_lsp.Server
open Why3_lsp.Scheduler
open Why3_lsp.Runtime

module Main : functor () -> sig end
 = functor () -> struct

(* files of the current task *)
let files = Queue.create ()

let quiet = ref false

let spec =
  let open Getopt in
  [KLong "quiet", Hnd0 (fun () -> quiet := true),
   " remove all printing to stdout"]

(* --help *)
let usage_str =
  "[<file.xml>|<f1.why> <f2.mlw> ...]\n\
   Launch a command-line interface for Why3."

let config, env =
  Whyconf.Args.initialize spec (fun f -> Queue.add f files) usage_str

let () =
  if Queue.is_empty files then
    Whyconf.Args.exit_with_usage usage_str;
  let fmt = if !quiet then str_formatter else std_formatter in
  fprintf fmt "Welcome to Why3 shell. Type 'help' for help.@.";
  let dir =
    try
      Server_utils.get_session_dir ~allow_mkdir:true files
    with Invalid_argument s ->
      eprintf "Error: %s@." s;
      Whyconf.Args.exit_with_usage usage_str
  in
  Queue.iter
    (fun f ->
      (* Sanitize the command line arguments so that they are always absolute *)
      let f = Sysutil.concat (Sys.getcwd ()) f in
      send_request (Add_file_req f))
    files;
  Server.init_server config env dir;
  Unix_scheduler.timeout ~ms:100
      (*a function that iterates over a list of notifications, applying treat_notification, returns true*) 
    (fun () -> List.iter
        (fun n -> treat_notification fmt n) (get_notified ()); 
        true);
  Unix_scheduler.main_loop (interp fmt)

end

let () = Whyconf.register_command "shell" (module Main)
