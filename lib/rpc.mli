exception Missing_Member of string
val member_opt : string -> Yojson__Basic.t -> Yojson__Basic.t option
val to_string_opt : Yojson__Basic.t option -> string option
val get_req_mem : string -> Yojson__Basic.t -> Yojson.Basic.t
val get_opt_mem : string -> Yojson__Basic.t -> Yojson.Basic.t option
module Constant :
  sig
    val jsonrpc : string
    val jsonrpcv : string
    val id : string
    val method_ : string
    val params : string
    val result : string
    val error : string
  end
module Id :
  sig
    type t = [ `Int of int | `String of string ]
    val yojson_of_t : t -> Yojson.Basic.t
    val t_of_yojson : Yojson__Basic.t -> t
  end
module Request :
  sig
    type t = { id : Id.t; method_ : string; params : Yojson.Basic.t option; }
    val t_of_yojson : Yojson__Basic.t -> t
    val yojson_of_t : t -> Yojson.Basic.t
  end
module Notification :
  sig
    type t = { method_ : string; params : Yojson.Basic.t option; }
    val t_of_yojson : Yojson__Basic.t -> t
    val yojson_of_t : t -> Yojson.Basic.t
  end
module Response :
  sig
    module Error :
      sig
        module Code :
          sig
            type t =
                ParseError
              | InvalidRequest
              | MethodNotFound
              | InvalidParams
              | InternalError
              | ServerErrorStart
              | ServerErrorEnd
              | ServerNotInitialized
              | UnknownErrorCode
              | RequestFailed
              | ServerCancelled
              | ContentModified
              | RequestCancelled
              | ServerAlreadyInitialized
              | Other of int
            val of_int : int -> t
            val to_int : t -> int
            val t_of_yojson : Yojson.Basic.t -> t
            val yojson_of_t : t -> [> `Int of int ]
          end
        type t = { code : Code.t; message : string; data : Yojson.Basic.t; }
        val construct_error :
          Code.t -> string -> Yojson.Basic.t -> ('a, t) result
        val yojson_of_t : t -> Yojson.Basic.t
        val t_of_yojson : Yojson.Basic.t -> t
      end
    type t = { id : Id.t; result : (Yojson.Basic.t, Error.t) Result.t; }
    type result = (Yojson.Basic.t, Error.t) Result.t
    val construct_response : Id.t -> result -> t
    val t_of_yojson : Yojson__Basic.t -> t
    val yojson_of_t : t -> Yojson.Basic.t
    val make : id:Id.t -> result:(Yojson.Basic.t, Error.t) Result.t -> t
    val ok : Id.t -> Yojson.Basic.t -> t
    val error : Id.t -> Error.t -> t
  end
val assert_jsonrpc_version : Yojson__Basic.t -> unit
module Packet :
  sig
    type call = [ `Notification of Notification.t | `Request of Request.t ]
    type bods =
        Notification of Notification.t
      | Request of Request.t
      | Response of Response.t
      | Batch_response of Response.t list
      | Batch_call of call list
    type t = { content_length : int; content_type : string; body : bods; }
    val yojson_of_t : bods -> Yojson.Basic.t
    val t_of_yojson : Yojson__Basic.t -> bods
    val t_of_yojson_single : Yojson__Basic.t -> bods
    val t_of_str : int -> string -> string -> t
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
  end
val add_to_requests :
  StringMap.key ->
  (Yojson.Basic.t -> Yojson.Basic.t) ->
  (Yojson.Basic.t -> Yojson.Basic.t) StringMap.t ->
  (Yojson.Basic.t -> Yojson.Basic.t) StringMap.t
val add_to_notifs :
  StringMap.key ->
  (Yojson.Basic.t -> unit) ->
  (Yojson.Basic.t -> unit) StringMap.t ->
  (Yojson.Basic.t -> unit) StringMap.t
