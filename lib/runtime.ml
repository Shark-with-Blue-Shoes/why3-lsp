open Yojson
open Printf
open Rpc_lib.Basic
open Mlsp_lib.Lifecycle

(* Global message queue and synchronization primitives *)
let msg_queue = Queue.create ()
let queue_mutex = Mutex.create ()
let queue_condition = Condition.create ()
let shutdown_flag = ref false (* A flag to signal consumer to stop *)

let all_request_calls = StringMap.empty |> add_to_calls "initialize" Initialize.respond

let call_request (req : Request.t) =
  let open Yojson.Basic in
  try
    match req.params with
    | Some params -> (StringMap.find req.method_ all_request_calls) params
    | None -> (StringMap.find req.method_ all_request_calls) `Null
  with
    Not_found -> Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error MethodNotFound "Request: Method called was not available" (from_string "{}"))) |> Response.yojson_of_t 
;;

let all_notifiation_calls = StringMap.empty

let call_notification (not : Notification.t) =
  let open Yojson.Basic in
  let _ = not.params in
  try
    match not.params with
    | Some params -> (StringMap.find not.method_ all_request_calls) params
    | None -> (StringMap.find not.method_ all_request_calls) `Null
  with
    Not_found -> Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error MethodNotFound "Notification: Method called was not available" (from_string "{}"))) |> Response.yojson_of_t 
;;

let respond_to_batch = 
  fun (call : Packet.call) -> 
  match call with 
    | `Notification not -> (call_notification not) |> Yojson.Basic.pretty_print Format.std_formatter
    | `Request req -> (call_request req) |> Yojson.Basic.pretty_print Format.std_formatter 

let interp buf =
  try
    let packet = Packet.t_of_str buf in
    match packet.body with 
    | Notification not -> (call_notification not) |> Yojson.Basic.pretty_print Format.std_formatter;
    | Request req -> (call_request req) |> Yojson.Basic.pretty_print Format.std_formatter;
    | Batch_call ls -> List.iter respond_to_batch ls
    | _ -> raise (Json_error "issue")
  with
    | Missing_Member err -> printf "Missing Member: %s\n\n%!" err
    | Yojson__Basic.Util.Type_error (x, _) -> printf "Type error: %s\n\n%!" x
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s\n\n%!" err
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
