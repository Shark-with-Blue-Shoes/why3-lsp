(*https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#uri*)
  type uri = [ `String of string | `Null ];;

val rooturi_of_json : Yojson.Basic.t -> uri

(*A type of request in workspace features, does not belong in basic_json_structs
https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspaceFolder
 *)
type workspaceFolder = { uri : uri; name : string; }

val opt_to_workspace_folders :
  Yojson.Basic.t option ->
  [ `Null | `WorkspaceFolders of workspaceFolder list ] option

(*A base type
https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lspAny
 *)
type lspAny =
    Null
  | String of string
  | LspObject of lspObject
  | LspArray of lspArray
  | Int of int
  | UInt of int
  | Decimal of float
  | Bool of bool
and lspObject = (string * lspAny) list
and lspArray = lspAny list

val lspAny_of_json_opt : Yojson__Basic.t option -> lspAny option

val json_of_lspAny_opt : lspAny option -> Yojson.Basic.t

(*Level of verbosity to report execution trace
https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#traceValue
 *)
type traceValue = Off | Messages | Verbose

val opt_to_trace : Yojson__Basic.t option -> traceValue option
