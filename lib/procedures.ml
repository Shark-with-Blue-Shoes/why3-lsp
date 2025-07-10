exception Method_Not_Found

open Rpc
open Yojson.Basic

let initialized = ref false;;

let initialize (json : Structured.t) : Response.t =
  let open Rpc.Response.Error.Code in
  match json with
  | `Null -> 
  if not !initialized then begin
    initialized := true;
    Response.construct_response (`Int 7) (Ok (from_string "{}"))
  end
  else
    Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error ServerAlreadyInitialized "Server was already initialized bozo!" (from_string "{}")))
  | _ -> Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error InvalidParams  "There are supposed to be no parameters bozo!" (from_string "{}")))
