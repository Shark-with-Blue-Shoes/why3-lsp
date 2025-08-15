type uri = string
type workspaceFolder = { uri : uri; name : string; }
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
type traceValue = Off | Messages | Verbose
val opt_to_trace : Yojson__Basic.t option -> traceValue option
val json_workspace_folder : Yojson.Basic.t -> workspaceFolder
val json_to_workspace_folders :
  Yojson.Basic.t -> [ `Null | `WorkspaceFolders of workspaceFolder list ]
val opt_to_workspace_folders :
  Yojson.Basic.t option ->
  [ `Null | `WorkspaceFolders of workspaceFolder list ] option
val json_to_lsp_any : Yojson.Basic.t -> lspAny
val opt_to_lsp_any : Yojson.Basic.t option -> lspAny option
val lspAny_to_json : lspAny -> Yojson.Basic.t
val optLspAny_to_json : lspAny option -> Yojson.Basic.t
val json_to_root_uri : Yojson.Basic.t -> uri
