exception Method_Not_Found

open Rpc

let initialized = ref false;;

let initialize () : Response.t =
  if not !initialized then begin
    print_string "Server has been initialized!\n\n";
    initialized := true;
    let res : Response.t = { 
      id = `Int 7;
      result = Ok (Yojson.Basic.from_string "{}");
    } in Response.print res; res
  end
  else begin
    print_string "Server was already initialized!\n\n";
  let res : Response.t = {
    id = `Int 7;
    result = Error {
      code = (Rpc.Response.Error.Code.of_int 10000); 
      message = "hello";  
      data = Yojson.Basic.from_string "{}";
                }
    } in Response.print res; res end;;
