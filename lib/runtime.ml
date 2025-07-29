open Yojson
open Printf
open Rpc_lib.Basic
open Lsp.Initialize
open Response.Error

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

let interp fmt buf =
  let _ = fmt in
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
  ;;

