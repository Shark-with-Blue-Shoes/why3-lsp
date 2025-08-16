type clientCapabilities = { l : bool; }

type serverCapabilities = {
  experimental : Basic_json_structs.lspAny option;
}

val initialized : bool ref

type request = {
  processId : [ `Int of int | `Null ];
  clientInfo : client_info option;
  locale : string option;
  rootPath : [ `Null | `String of string ] option;
  rootUri : Basic_json_structs.uri;
  initializationOptions : Basic_json_structs.lspAny option;
  clientCapabilities : clientCapabilities;
  trace : Basic_json_structs.traceValue option;
  workspaceFolders :
    [ `Null
    | `WorkspaceFolders of
        Basic_json_structs.workspaceFolder list ]
    option;
}
and client_info = { name : string; version : string option; }

val json_to_pid : Yojson.Basic.t -> [ `Int of int | `Null ]

val json_to_client_info : Yojson__Basic.t -> client_info

val opt_to_client_info : Yojson.Basic.t option -> client_info option

val json_to_root_path : Yojson.Basic.t -> [ `Null | `String of string ]

val opt_to_root_path :
  Yojson.Basic.t option -> [ `Null | `String of string ] option

val request_of_yojson : Yojson__Basic.t -> request

type response = {
  capabilities : serverCapabilities;
  serverInfo : serverInfo option;
}
and serverInfo = { name : string; version : string option; }

type error = { retry : bool; }

val yojson_of_result :
  response -> [> `Assoc of (string * Yojson.Basic.t) list ]

val yojson_of_error_data : error -> Yojson.Basic.t

val resp_to_json :
  Rpc.Id.t -> (response, error) result -> Yojson.Basic.t

val initialize : [< `Int of 'a | `Null ] -> 'b -> (response, error) Result.t

val respond : Yojson__Basic.t -> Yojson.Basic.t
