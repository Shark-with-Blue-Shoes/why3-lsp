exception Missing_Member of string

open Yojson.Basic.Util

let has_id json = 
  let open Yojson.Basic.Util in
  match json |> member "id" with
  | `Null -> false
  | _ -> true

(*This gets a required member*)
let get_req_mem json name : Yojson.Basic.t = 
  let mem = member name json in
  match mem with
  | `Null -> raise (Missing_Member (Printf.sprintf "%s is missing!\n" name))
  | _ -> mem

(*This gets an option member*)
let get_opt_mem json name : Yojson.Basic.t = 
  let mem = member name json in
  match mem with
  | `Null -> `Null
  | m -> m

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


module Structured = struct
  type t =
    [ `Assoc of (string * Yojson.Basic.t) list
    | `List of Yojson.Basic.t list
    | `Null 
    ]
    
  let t_of_yojson params : t =
    match params with
    | `Assoc x -> `Assoc x
    | `List ls ->  `List ls
    | `Null -> `Null
    | err -> raise (Type_error ("Not correct type of param ", err))

  let yojson_of_t (t : t) : Yojson.Basic.t =
    match t with 
    | `Assoc x -> `Assoc x
    | `List ls ->  `List ls
    | `Null -> `Null
    
end

module Request = struct
  type t =
    { id : Id.t
    ; method_ : string
    ; params : Structured.t
    }

  let t_of_yojson json : t = 
    {
      id = get_req_mem json "id" |> Id.t_of_yojson;
      method_ = get_req_mem json "method" |> to_string ;
      params = get_opt_mem json "params" |> Structured.t_of_yojson
    }

  let yojson_of_t t : Yojson.Basic.t =
    `Assoc ["id",  (Id.yojson_of_t t.id); "method" , (`String t.method_); "params", (Structured.yojson_of_t t.params)] 

  let print t = 
    Printf.printf "%s\n%!" (Yojson.Basic.to_string (yojson_of_t t));;

end

module Notification = struct 
  type t =
    { method_ : string
    ; params : Structured.t
    }
  
  let t_of_yojson json : t = 
    {
      method_ = get_req_mem json "method" |> to_string ;
      params = get_opt_mem json  "params" |> Structured.t_of_yojson
    }

  let yojson_of_t t : Yojson.Basic.t =
    `Assoc ["method" , (`String t.method_); "params", (Structured.yojson_of_t t.params)]
     
  let print t = 
    Printf.printf "%s\n%!" (Yojson.Basic.to_string (yojson_of_t t));;

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

    let construct_error code msg _data : t = 
      {
        code = code;
        message = msg;
        data = _data
      };;
   
    let yojson_of_t t : Yojson.Basic.t =
      `Assoc ["code" , (`Int (Code.to_int t.code)); "message", (`String t.message); "data", t.data]
    
    let t_of_yojson (json : Yojson.Basic.t) : t =
      {
        code = get_req_mem json "code" |> Code.t_of_yojson;
        message = get_req_mem json "message" |> to_string;
        data = get_req_mem json "data"
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

  (*This is a pretty print function, which is prettier than the standard pprint_string (Yojson.Basic.to_string x)*)
  let print t =   
    let id_str = Id.to_str t.id in
    let res_str = match t.result with
      (*If the result was Ok, print a json with that has result: ... *)
    | Ok js -> let res_str = Yojson.Basic.to_string js in  
    let str = Printf.sprintf "Response: {\n id: %s \n result: %s \n}\n\n\n" id_str res_str in str
      (*If the result was an Error, print a json with that has error: ... *)
    | Error err -> 
      let code_str = Error.Code.to_int err.code in 
      let data_str = Yojson.Basic.to_string err.data in 
      let err_str = Printf.sprintf "Error: {\n code: %d \n message: %s \n data: %s \n   }" code_str err.message data_str in
      let str = Printf.sprintf "Response: {\n id: %s \n error: %s \n}\n\n\n" id_str err_str in str
        in
    print_string res_str;;

  let t_of_yojson json : t =
    let res_opt = get_opt_mem json "result" in
      let res = match res_opt with
    | `Null -> Error (get_req_mem json "error" |> Error.t_of_yojson)
    | _ -> Ok res_opt in
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
    let jsonrpc = get_req_mem json "jsonrpc" |> to_string in
    if not (String.equal jsonrpc Constant.jsonrpcv)
    then
      raise (Yojson.Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

module Packet = struct
  type t =
    | Notification of Notification.t
    | Request of Request.t
    | Response of Response.t
end
