exception Method_Not_Found

open Rpc
open Yojson.Basic

let initialized = ref false;;

let initialize () : Response.t =
  let open Rpc.Response.Error.Code in
  if not !initialized then begin
    initialized := true;
    Response.construct_response (`Int 7) (Ok (from_string "{}"))
  end
  else
    Response.construct_response (`Int 7) 
    (Error (Response.Error.construct_error ServerAlreadyInitialized "Server was already initialized bozo!" (from_string "{}")))
