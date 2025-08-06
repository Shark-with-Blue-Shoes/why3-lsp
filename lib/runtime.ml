open Yojson
open Printf
open Rpc
open Lsp.Initialize
open Response.Error

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
    | `Request req -> call_request req |> Basic.pretty_to_channel stdout
    
let interp cnt_len cnt_typ buf =
  try
    let packet = Packet.t_of_str cnt_len cnt_typ buf in
    match packet.body with 
    | Notification not -> call_notification not;
    | Request req -> call_request req |> Basic.pretty_to_channel stdout;
    | Batch_call ls -> List.iter respond_to_batch ls;
    | _ -> raise (Json_error "issue");
  with
    | Missing_Member err -> printf "Missing Member: %s\n\n%!" err
    | Yojson__Basic.Util.Type_error (x, y) -> printf " %s experienced a Type error of: %s\n\n%!" (Basic.to_string y) x 
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s\n\n%!" err
    | Rgx.Rgx_failure err -> printf "Regex error: %s\n\n%!" err
    | _ as e -> printf "Strange error: %s\n%!" (Printexc.to_string e)
  ;;

let oc = open_out "why3-lsp.log";;

let get_content_length str = 
  try
    let rgx = Re.Perl.compile_pat "\\d+" in
      let mtch = Re.exec rgx str in
        let content_len_unsan = Re.Group.get mtch 0 in 
        let content_len = (int_of_string content_len_unsan) in content_len
  with 
  | e -> raise (Rgx.Rgx_failure (sprintf "Error in getting content length: %s\n" (Printexc.to_string e)));;

let logger cnt_len str bytes_read = 
  let time = Unix.time () |> Unix.localtime in
  (sprintf "[INPUT] [TIME: %d:%d:%d] [BYTES READ: %d] [CONTENT-LENGTH: %d] DATA: %s\n%!"
      cnt_len time.tm_hour time.tm_min time.tm_sec bytes_read str) |>
      output_string oc;
    flush oc;;

(*Gets body and amount of bytes read*)
let get_body cnt_len = 
  let byt = Bytes.create cnt_len in
    let bytes_read = In_channel.input stdin byt 0 cnt_len in
    (Bytes.to_string byt, bytes_read);;

let rec loop () =
  try
    let str = read_line () in
      let cnt_len = get_content_length str in
        (*This read_line past the extra \r\n, and straight to the body so that \r\n isn't read by get_body*)
        let _ = read_line () in
          let (body, bytes_read) = get_body cnt_len in
            interp cnt_len "type" body;
            logger (get_content_length str) body bytes_read;
        loop ()
  with 
  e -> Printexc.to_string e |> eprintf "Fatal error: %s\n";;

