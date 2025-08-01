open Yojson
open Printf
open Rpc_lib.Basic
open Lsp.Initialize
open Response.Error

(*This manages the requests/notifs*)
let list_requests: Request.t list ref = ref []

let get_requests () : Request.t list =
  if List.length !list_requests > 0 then
  let l = List.rev !list_requests in
  list_requests := [];
    l else [];;

let send_request r =
  list_requests := r :: !list_requests

let notification_list: Notification.t list ref = ref []

let send_notif n =
  notification_list := n :: !notification_list

let get_notified () : Notification.t list =
  if List.length !notification_list > 0 then
  let l = List.rev !notification_list in
  notification_list := [];
    l else [];;

(*These manage the procedure calls for the requests/notifs*)
let all_request_calls = StringMap.empty |> add_to_calls "initialize" Initialize.respond

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

let all_notifiation_calls = StringMap.empty

let call_notification (not : Notification.t) =
  let open Yojson.Basic in
  try
    match not.params with
    | Some params -> (StringMap.find not.method_ all_request_calls) params
    | None -> (StringMap.find not.method_ all_request_calls) `Null
  with
    Not_found -> from_string "{}" |> construct_error Code.MethodNotFound "Method was not found bozo" |>
          Response.construct_response (`Int 10) |> Response.yojson_of_t 
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
    | Notification not -> send_notif not;
    | Request req -> send_request req;
    | Batch_call ls -> List.iter respond_to_batch ls;
    | _ -> raise (Json_error "issue");
  with
    | Missing_Member err -> printf "Missing Member: %s\n\n%!" err
    | Yojson__Basic.Util.Type_error (x, y) -> printf " %s experienced a Type error of: %s\n\n%!" (Basic.to_string y) x 
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s\n\n%!" err
    | Rpc_lib.Rgx.Rgx_failure err -> printf "Regex error: %s\n\n%!" err
    | _ as e -> printf "Strange error: %s\n%!" (Printexc.to_string e)
  ;;

