open Why3_lsp.Rpc 
open Queue
open Thread

let msg_queue = Queue.create ()

let rec server () = 
  let input = read_line () in
    Queue.push input msg_queue;
    server ();;

let () = 
  Queue.push "{\"version\": \"2.0\", \"num\": 5}" msg_queue; 
  let _ = Thread.create (fun a -> Printf.printf "This is the Queue: %s\n" a) (Queue.pop msg_queue) in
  server ();;
