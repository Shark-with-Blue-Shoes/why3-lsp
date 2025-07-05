open Yojson
open Yojson.Basic.Util
open Printf

module Constant = struct
  let jsonrpc = "jsonrpc"
  let jsonrpcv = "2.0"
  let id = "id"
  let method_ = "method"
  let params = "params"
  let result = "result"
  let error = "error"
end

let assert_jsonrpc_version json =

  let jsonrpc = json |> member "version" |> to_string in
  if not (String.equal jsonrpc Constant.jsonrpcv)
  then
    raise (Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

let is_even num =  
    match num mod 2 with 
    | 0 ->  printf "%d is even!\n" num
    | 1 -> printf "%d is odd!\n" num
    | _ -> assert false

let interp buf = 
  let json = Basic.from_string buf in
  try
    assert_jsonrpc_version json;
    let num = json |> member "num" |> to_int in 
    is_even num;
  with
    | Yojson__Basic.Util.Type_error (x, _) -> printf "Type error: %s\n" x
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s\n" err
    | _ as e -> printf "strange error: %s\n" (Printexc.to_string e)
