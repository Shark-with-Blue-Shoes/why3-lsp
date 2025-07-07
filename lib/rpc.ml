
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
  open Yojson.Basic.Util

  type t =
    [ `String of string
    | `Int of int
    ]

  let yojson_of_t (t : t) : Yojson.Basic.t = 
    match t with
    | `Int i -> `Int i
    | `String str -> `String str

  let t_of_yojson json : t =
    let get_id id : t =
      match id with
      | `Int i -> `Int i
      | `String str -> `String str
      | err -> raise (Type_error ("Not correct type of id ", err))
    in
    json |> member "id" |> get_id

end


module Structured = struct
  open Yojson.Basic.Util
  type t =
    [ `Assoc of (string * Yojson.Basic.t) list
    | `List of Yojson.Basic.t list
    | `Null
    ]
    
  let t_of_yojson json : t =
    let get_structured (params : Yojson.Basic.t) : t =
      match params with
      | `Assoc x -> `Assoc x
      | `List ls ->  `List ls
      | `Null -> `Null
      | err -> raise (Type_error ("Not correct type of param ", err)) in
    json |> member "param" |> get_structured

  let yojson_of_t (t : t) : Yojson.Basic.t =
    match t with 
    | `Assoc x -> `Assoc x
    | `List ls ->  `List ls
    | `Null -> `Null
    
end

module Request = struct
  open Yojson.Basic.Util
  type t =
    { id : Id.t
    ; method_ : string
    ; params : Structured.t
    }

  let t_of_yojson json : t = 
    {
      id = Id.t_of_yojson json;
      method_ = json |> member "method" |> to_string ;
      params = Structured.t_of_yojson json
    }

  let yojson_of_t t : Yojson.Basic.t =
    `Assoc (["id",  (Id.yojson_of_t t.id); "method" , (`String t.method_); "params", (Structured.yojson_of_t t.params)])
     
end

module Notification = struct 
  open Yojson.Basic.Util
  type t =
    { method_ : string
    ; params : Structured.t
    }

  let t_of_yojson json : t = 
    {
      method_ = json |> member "method" |> to_string ;
      params = Structured.t_of_yojson json
    }

  let yojson_of_t t : Yojson.Basic.t =
    `Assoc (["method" , (`String t.method_); "params", (Structured.yojson_of_t t.params)])
     
end

let assert_jsonrpc_version json =
  let open Yojson.Basic.Util in
    let jsonrpc = json |> member "version" |> to_string in
    if not (String.equal jsonrpc Constant.jsonrpcv)
    then
      raise (Yojson.Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

