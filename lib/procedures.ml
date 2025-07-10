exception Method_Not_Found

open Rpc

let initialized = ref false;;

let initialize () : Response.t =
  let open Rpc.Response.Error.Code in
  if not !initialized then begin
    initialized := true;
    let res : Response.t = { 
      id = `Int 7;
      result = Ok (Yojson.Basic.from_string "{}");
    } in res
  end
  else begin
  let res : Response.t = {
    id = `Int 7;
    result = Error {
      code = ServerAlreadyInitialized; 
      message = "Server was already initialized bozo!";  
      data = Yojson.Basic.from_string "{}";
                }
    } in res end;;
