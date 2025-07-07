
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
  type t =
    [ `String of string
    | `Int of int
    | `Null
    ]

  let yojson_of_t (t : t) : Yojson.Basic.t = 
    match t with
    | `Null -> `Null
    | `Int i -> `Int i
    | `String str -> `String str

  let t_of_yojson json =
    let open Yojson.Basic.Util in
    let get_t id =
      match id with
      | `Null -> `Null
      | `Int i -> `Int i
      | `String str -> `String str
      | err -> raise (Type_error ("not correct type of id ", err))
    in
    json |> member "id" |> get_t

end

module Notification = struct

end

module Request = struct
end

let assert_jsonrpc_version json =
  let open Yojson.Basic.Util in
    let jsonrpc = json |> member "version" |> to_string in
    if not (String.equal jsonrpc Constant.jsonrpcv)
    then
      raise (Yojson.Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

