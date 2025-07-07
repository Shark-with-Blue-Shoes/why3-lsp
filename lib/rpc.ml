open Yojson
open Yojson.Basic.Util

module Constant = struct
  let jsonrpc = "jsonrpc"
  let jsonrpcv = "2.0"
  let id = "id"
  let method_ = "method"
  let params = "params"
  let result = "result"
  let error = "error"
end

module Id = struct
end

module Notification = struct
end

module Request = struct
end

let assert_jsonrpc_version json =
  let jsonrpc = json |> member "version" |> to_string in
  if not (String.equal jsonrpc Constant.jsonrpcv)
  then
    raise (Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

