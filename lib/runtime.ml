exception Rgx_failure of string

open Yojson
open Printf
open Rpc

(* Global message queue and synchronization primitives *)
let msg_queue = Queue.create ()
let queue_mutex = Mutex.create ()
let queue_condition = Condition.create ()
let shutdown_flag = ref false (* A flag to signal consumer to stop *)

let has_id json = 
  let open Yojson.Basic.Util in
  match json |> member "id" with
  | `Null -> false
  | _ -> true


let get_header str = 
  try 
  let headers_rgx = "(.*)\\\\r\\\\n" in
    let rgx = Re.Perl.compile_pat headers_rgx in 
      let mtch = Re.exec rgx str in
        let headers = Re.Group.get mtch 1 in headers
  with
    e -> raise (Rgx_failure (sprintf "Error in getting headers: %s\n" (Printexc.to_string e)));;

let get_json str = 
  try 
    let json_rgx = "\\{.*\\}" in
      let rgx = Re.Perl.compile_pat json_rgx in 
        let mtch = Re.exec rgx str in 
          let json = Re.Group.get mtch 0 in json
  with 
    e -> raise (Rgx_failure (sprintf "Error in getting json: %s\n" (Printexc.to_string e)));;

let split_header_json str = 
    let json = get_json str in
      let headers = get_header str in
              (headers, json)

let get_content_len str = 
  try
    let rgx = Re.Perl.compile_pat "Content-Length: (\\d+)\\\\r\\\\n" in
      let mtch = Re.exec rgx str in
        let content_len = Re.Group.get mtch 1 in content_len
  with 
  | e -> raise (Rgx_failure (sprintf "Error in getting content length: %s\n" (Printexc.to_string e)));;

let get_content_typ str = 
  try
    let rgx = Re.Perl.compile_pat "Content-Type: \"(.*)\"\\\\r\\\\n" in 
      let mtch = Re.exec rgx str in 
        let content_type = Re.Group.get mtch 1 in content_type
  with 
  | e -> raise (Rgx_failure (sprintf "Error in getting content length: %s\n" (Printexc.to_string e)));;

let split_header str = 
  let content_len = get_content_len str in 
    let content_typ = get_content_typ str in 
      (content_len, content_typ)

let interp buf =
  try
  let (header_str, json) = split_header_json buf in
    let (content_length, content_type) = split_header header_str in
    printf "Content Length: %s, Content Type: %s\n%!" content_length content_type;
    let json = Basic.from_string json in
    if has_id json then
      let req = Request.t_of_yojson json in
      printf "{id: %s, method: %s, params: %s}\n\n%!" (Id.to_str req.id) req.method_ (Basic.to_string (Structured.yojson_of_t req.params))
    else
      let not = Notification.t_of_yojson json in 
      printf "{method: %s, params: %s}\n\n%!" not.method_ (Basic.to_string (Structured.yojson_of_t not.params))
  with
    | Missing_Member err -> printf "Missing Member: %s\n%!" err
    | Yojson__Basic.Util.Type_error (x, _) -> printf "Type error: %s\n%!" x
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s\n%!" err
    | Rgx_failure err -> printf "Rgx_failure: %s\n%!" err
    | _ as e -> printf "Strange error: %s\n%!" (Printexc.to_string e)

(* Consumer thread function: Continuously pops messages from the queue and processes them *)
let consumer_thread_func () =
  try
    while not !shutdown_flag || not (Queue.is_empty msg_queue) do
      let msg =
        Mutex.lock queue_mutex; (* Acquire mutex before accessing the queue *)
        let rec wait_for_message () =
          if Queue.is_empty msg_queue then (
            (* If queue is empty and not shutting down, wait for a signal *)
            if not !shutdown_flag then (
              Condition.wait queue_condition queue_mutex;
              wait_for_message () (* Re-check after being signaled *)
            ) else (
              (* If shutting down and queue is empty, break out *)
              raise End_of_file
            )
          ) else (
            Queue.pop msg_queue (* Pop message if available *)
          )
        in
        let m = wait_for_message () in
        Mutex.unlock queue_mutex; (* Release mutex after accessing the queue *)
        m
      in
      interp msg;
    done
  with End_of_file ->
    Printf.printf "[Consumer] Detected shutdown signal and queue is empty. Exiting.\n%!"
  | e ->
    Printf.eprintf "[Consumer] An unexpected error occurred: %s\n%!" (Printexc.to_string e);
    Mutex.unlock queue_mutex; (* Ensure mutex is released on error *)
    raise Thread.Exit

(* Producer (main server loop) function: Reads input and pushes to the queue *)
let rec server_producer () =
  match In_channel.input_line stdin with
  | Some line ->
      Mutex.lock queue_mutex; (* Acquire mutex before accessing the queue *)
      Queue.push line msg_queue;
      Condition.signal queue_condition; (* Signal consumer that new data is available *)
      Mutex.unlock queue_mutex; (* Release mutex *)
      server_producer () (* Continue reading input *)
  | None ->
      (* End of input (Ctrl+D) reached. Signal consumer to shut down. *)
      Printf.printf "[Producer] End of input (EOF) reached. Signaling consumer to shut down.\n\n%!";
      Mutex.lock queue_mutex;
      shutdown_flag := true; (* Set shutdown flag *)
      Condition.signal queue_condition; (* Signal consumer one last time *)
      Mutex.unlock queue_mutex;
      () (* Exit the producer loop *)
