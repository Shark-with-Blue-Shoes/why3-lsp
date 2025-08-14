open Yojson
open Printf
open Rpc
open Lsp.Initialize
open Response.Error
open Rgx
open Logger

(*These manage the procedure calls for the requests/notifs*)
let all_request_calls = StringMap.empty |> add_to_requests "initialize" Initialize.respond

let call_request (req : Request.t) =
  let open Yojson.Basic in
  try
    match req.params with
    | Some params -> (StringMap.find req.method_ all_request_calls) params
    | None -> (StringMap.find req.method_ all_request_calls) `Null
  with
    Not_found -> from_string "{}" |> construct_error Code.MethodNotFound "Method was not found bozo" |>
          Response.construct_response (`Int 10) |> Response.yojson_of_t
;;

let all_notification_calls = StringMap.empty

let call_notification (not : Notification.t) =
  try
    match not.params with
    | Some args -> (StringMap.find not.method_ all_notification_calls) args
    | None -> (StringMap.find not.method_ all_notification_calls) `Null
  with
    Not_found -> ();;

let respond_to_batch = 
  fun (call : Packet.call) -> 
  match call with 
    | `Notification not -> call_notification not
    | `Request req -> call_request req |> Basic.to_string |> log_out;;
    
let interp cnt_len cnt_typ buf =
    let packet = Packet.t_of_str cnt_len cnt_typ buf in
    match packet.body with 
    | Notification not -> call_notification not;
    | Request req -> call_request req |> Basic.to_string |> log_out;
    | Batch_call ls -> List.iter respond_to_batch ls;
    | Batch_response _ -> ()
    | Response _ -> ()
  ;;

(*Gets body and amount of bytes read*)
let get_body cnt_len = 
  let byt = Bytes.create cnt_len in
    let bytes_read = In_channel.input stdin byt 0 cnt_len in
    (Bytes.to_string byt, bytes_read);;

let rec loop () =
  try
    let str = read_line () in
      let cnt_len = get_content_len str in
        (*This read_line past the extra \r\n, and straight to the body so that \r\n isn't read by get_body*)
        let _ = read_line () in
          let (body, bytes_read) = get_body cnt_len in
            interp cnt_len "type" body;
            log_in cnt_len body bytes_read;
        loop ()
  with 
  e -> Printexc.to_string e |> eprintf "Error: %s\n";;

