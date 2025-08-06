exception Missing_Member of string

open Yojson.Basic.Util

let member_opt k = path [k]

(*This gets a required member*)
let get_req_mem name json : Yojson.Basic.t = 
  let mem = member_opt name json in
  match mem with
  | Some elem  -> elem
  | None -> raise (Missing_Member (Printf.sprintf "A obligatory flag called %s is missing!\n" name))

(*This gets an option member*)
let get_opt_mem name json : Yojson.Basic.t option = 
  member_opt name json;;

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
    ]

  let yojson_of_t (t : t) : Yojson.Basic.t = 
    match t with
    | `Int i -> `Int i
    | `String str -> `String str

  let t_of_yojson json : t =
    match json with
    | `Int i -> `Int i
    | `String str -> `String str
    | `Null -> raise (Missing_Member "Id is missing!")
    | err -> raise (Type_error ("Not correct type of id ", err))

  let to_str (t : t) =
  match t with
    | `String x -> x
    | `Int x -> Printf.sprintf "%d" x

end

module Request = struct
  type t =
    { id : Id.t
    ; method_ : string
    ; params : Yojson.Basic.t option
    }

  let t_of_yojson json : t = 
    {
      id = get_req_mem "id" json |> Id.t_of_yojson;
      method_ = get_req_mem "method" json |> to_string ;
      params = get_opt_mem "params" json
    }

  let yojson_of_t t : Yojson.Basic.t =
    match t.params with
    | None -> `Assoc ["id",  (Id.yojson_of_t t.id); "method" , (`String t.method_);]
    | Some params -> `Assoc ["id",  (Id.yojson_of_t t.id); "method" , (`String t.method_); "params", params] ;;
end

module Notification = struct 
  type t =
    { method_ : string
    ; params : Yojson.Basic.t option
    }
  
  let t_of_yojson json : t = 
    {
      method_ = get_req_mem "method" json |> to_string ;
      params = get_opt_mem "params" json 
    }

  let yojson_of_t t : Yojson.Basic.t =
    match t.params with
    | None -> `Assoc ["method" , (`String t.method_);]
    | Some params -> `Assoc ["method" , (`String t.method_); "params", params] ;;

end

module Response = struct
  module Error = struct
    module Code = struct
      type t =
      | ParseError
      | InvalidRequest
      | MethodNotFound
      | InvalidParams
      | InternalError
      (* the codes below are LSP specific *)
      | ServerErrorStart
      | ServerErrorEnd
      | ServerNotInitialized
      | UnknownErrorCode
      | RequestFailed
      | ServerCancelled
      | ContentModified
      | RequestCancelled
      (* all other codes are custom *)
      | ServerAlreadyInitialized
      | Other of int
     
      let of_int = function
      | -32700 -> ParseError
      | -32600 -> InvalidRequest
      | -32601 -> MethodNotFound
      | -32602 -> InvalidParams
      | -32603 -> InternalError
      | -32099 -> ServerErrorStart
      | -32000 -> ServerErrorEnd
      | -32002 -> ServerNotInitialized
      | -32001 -> UnknownErrorCode
      | -32800 -> RequestCancelled
      | -32801 -> ContentModified
      | -32802 -> ServerCancelled
      | -32803 -> RequestFailed
      | code -> Other code
      ;;
     
      let to_int = function
      | ParseError -> -32700
      | InvalidRequest -> -32600
      | MethodNotFound -> -32601
      | InvalidParams -> -32602
      | InternalError -> -32603
      | ServerErrorStart -> -32099
      | ServerErrorEnd -> -32000
      | ServerNotInitialized -> -32002
      | UnknownErrorCode -> -32001
      | RequestCancelled -> -32800
      | ContentModified -> -32801
      | ServerCancelled -> -32802
      | RequestFailed -> -32803
      | ServerAlreadyInitialized -> -33333
      | Other code -> code
      ;;
     
      let t_of_yojson (json : Yojson.Basic.t) =
        match json with
        | `Int i -> of_int i
        | err -> raise (Type_error ("Not correct type of param ", err))
        ;;
     
          let yojson_of_t t = `Int (to_int t)
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Yojson.Basic.t
      }

    let construct_error code msg _data = 
      Error {
        code = code;
        message = msg;
        data = _data
      };;
   
    let yojson_of_t t : Yojson.Basic.t =
      `Assoc ["code" , (`Int (Code.to_int t.code)); "message", (`String t.message); "data", t.data]
    
    let t_of_yojson (json : Yojson.Basic.t) : t =
      {
        code = get_req_mem "code" json |> Code.t_of_yojson;
        message = get_req_mem "message" json |> to_string;
        data = get_req_mem "data" json 
    }
  end

  type t =
    { id : Id.t;
      result : (Yojson.Basic.t, Error.t) Result.t
    }

  let construct_response id (res : (Yojson.Basic.t, Error.t) Result.t) =
    {
      id = id;
      result = res 
    }

  let t_of_yojson json : t =
    let res_opt = get_opt_mem "result" json  in
    let res = match res_opt with
    | None -> Error (get_req_mem "error" json |> Error.t_of_yojson)
    | Some result -> Ok result in
    construct_response (Id.t_of_yojson json) res;;

    let yojson_of_t t : Yojson.Basic.t = 
      match t.result with
      | Error x -> `Assoc ["id" , Id.yojson_of_t t.id; "error", Error.yojson_of_t x]
      | Ok x -> `Assoc ["id" , Id.yojson_of_t t.id; "result", x];;

    let make ~id ~result = { id; result };;
    let ok id result = make ~id ~result:(Ok result);;
    let error id error = make ~id ~result:(Error error);;
end


let assert_jsonrpc_version json =
    let jsonrpc = get_req_mem "jsonrpc" json |> to_string in
    if not (String.equal jsonrpc Constant.jsonrpcv)
    then
      raise (Yojson.Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

module Packet = struct
  type call = [ `Request of Request.t | `Notification of Notification.t ];;

  type bods =
    | Notification of Notification.t
    | Request of Request.t
    | Response of Response.t
    | Batch_response of Response.t list
    | Batch_call of call list;;

  type t = {
    content_length: int;
    content_type: string;
    body: bods
  }

  let yojson_of_t = function
    | Notification r -> Notification.yojson_of_t r
    | Request r -> Request.yojson_of_t r
    | Response r -> Response.yojson_of_t r
    | Batch_response r -> `List (List.map Response.yojson_of_t r)
    | Batch_call r ->
      `List
        (List.map (function
           | `Request r -> Request.yojson_of_t r
           | `Notification r -> Notification.yojson_of_t r) r)
  ;;

  let t_of_yojson json =
    assert_jsonrpc_version json;
    try 
    let req = Request.t_of_yojson json in
      Request req
      with _ -> try
        let not = Notification.t_of_yojson json in
        Notification not
        with _ -> try
          let res = Response.t_of_yojson json in 
          Response res 
          with _ -> raise (Yojson.Json_error "invalid packet");;

  let t_of_yojson_single json =
    match json with
    | `Assoc _ -> t_of_yojson json
    | _ -> raise (Yojson.Json_error "invalid packet");;

  let t_of_str content_length content_type (buf : string) =
    let json = Yojson.Basic.from_string buf in
    let select_packet = match json with
    | `List [] -> raise (Yojson.Json_error "invalid packet")
    | `List (x :: xs) ->  
      (* we inspect the first element to see what we're dealing with *)
      let x =
        match x with
        | `Assoc _ -> t_of_yojson x
        | _ -> raise (Yojson.Json_error "invalid packet")
      in
      (match
         match x with
         | Notification x -> `Call (`Notification x)
         | Request x -> `Call (`Request x)
         | Response r -> `Response r
         | _ -> raise (Yojson.Json_error "invalid packet")
       with
       | `Call x ->
         Batch_call
           (x
            :: List.map (fun call ->
              let x = t_of_yojson_single call in
              match x with
              | Notification n -> `Notification n
              | Request n -> `Request n
              | _ -> raise (Yojson.Json_error "invalid packet")) xs)
       | `Response x ->
         Batch_response
           (x
            :: List.map (fun resp ->
              let resp = t_of_yojson_single resp in
              match resp with
              | Response n -> n
              | _ -> raise (Yojson.Json_error "invalid packet")) xs))
      | _ -> t_of_yojson_single json in
      {
      content_length = content_length;
      content_type = content_type;
      body = select_packet
    };;
end

module StringMap :
  sig
    type key = string
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
  end = Map.Make(String);;


let add_to_requests str (response : Yojson.Basic.t -> Yojson.Basic.t) map : ('a -> 'b) StringMap.t= 
  StringMap.add str (fun params -> response params) map;;

let add_to_notifs str (action : Yojson.Basic.t -> unit) map : ('a -> 'b) StringMap.t= 
  StringMap.add str (fun params -> action params) map;;

