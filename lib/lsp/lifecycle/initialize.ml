open Yojson.Basic.Util
open Yojson.Basic
open Rpc
open Basic_json_structs

type clientCapabilities = {
  (*synchronization: textDocumentSyncClientCapabilities option;

	completion: completionClientCapabilities option;

	hover: hoverClientCapabilities option;

	signatureHelp: signatureHelpClientCapabilities option;

	declaration: declarationClientCapabilities option;

	definition: definitionClientCapabilities option;

	typeDefinition: typeDefinitionClientCapabilities option;

	implementation: implementationClientCapabilities option;

	references: referenceClientCapabilities option;

	documentHighlight: documentHighlightClientCapabilities option;

  documentSymbol: documentSymbolClientCapabilities option;

	codeAction: codeActionClientCapabilities option;

	codeLens: codeLensClientCapabilities option;

	documentLink: documentLinkClientCapabilities option;

	colorProvider: documentColorClientCapabilities option;

	formatting: documentFormattingClientCapabilities option;

	rangeFormatting: documentRangeFormattingClientCapabilities option;

	onTypeFormatting: documentOnTypeFormattingClientCapabilities option;

	rename: renameClientCapabilities option;

	publishDiagnostics: publishDiagnosticsClientCapabilities option;

	foldingRange: foldingRangeClientCapabilities option;

  selectionRange: selectionRangeClientCapabilities option;

	linkedEditingRange: linkedEditingRangeClientCapabilities option;

	callHierarchy: callHierarchyClientCapabilities option;

	semanticTokens: semanticTokensClientCapabilities option;

	moniker: monikerClientCapabilities option;

	typeHierarchy: typeHierarchyClientCapabilities option;

	inlineValue: inlineValueClientCapabilities option;

	inlayHint: inlayHintClientCapabilities option;

	diagnostic: DiagnosticClientCapabilities option;*)
  l: bool
};;


type serverCapabilities = {
  (*positionEncoding: positionEncodingKind option;

  textDocumentSync: [`TextDocumentSyncOptions | `TextDocumentSyncKind] option;

	notebookDocumentSync: [`NotebookDocumentSyncOptions
    | `NotebookDocumentSyncRegistrationOptions] option;

  completionProvider: completionOptions option;

  hoverProvider: [`Bool of bool | `HoverOptions] option;

  signatureHelpProvider: [`SignatureHelpOptions];

  declarationProvider: [`Bool of bool | `DeclarationOptions
    | `DeclarationRegistrationOptions] option;

  definitionProvider: [`Bool of bool | `DefinitionOptions] option;

	typeDefinitionProvider: [`Bool of bool | `TypeDefinitionOptions
    | `TypeDefinitionRegistrationOptions] option;

	implementationProvider: [`Bool of bool | `ImplementationOptions
    | `ImplementationRegistrationOptions] option;

  referencesProvider: [`Bool of bool | `ReferenceOptions] option;

  documentHighlightProvider: [`Bool of bool | `DocumentHighlightOptions] option;

  documentSymbolProvider: [`Bool of bool | `DocumentSymbolOptions] option;

  codeActionProvider: [`Bool of bool | `CodeActionOptions] option;

  codeLensProvider: codeLensOptions option;

	documentLinkProvider: documentLinkOptions option;

	colorProvider: [`Bool of bool | `DocumentColorOptions
    | `DocumentColorRegistrationOptions] option;

  documentFormattingProvider: [`Bool of bool | `DocumentFormattingOptions] option;

  documentRangeFormattingProvider: [`Bool of bool | `DocumentRangeFormattingOptions] option;

	documentOnTypeFormattingProvider: documentOnTypeFormattingOptions option;

  renameProvider: [`Bool of bool | `RenameOptions];

	foldingRangeProvider: [`Bool of bool | `FoldingRangeOptions
      | `FoldingRangeRegistrationOptions] option;

	executeCommandProvider: executeCommandOptions option;

	selectionRangeProvider: [`Bool of bool | `SelectionRangeOptions
    | `SelectionRangeRegistrationOptions] option;

	linkedEditingRangeProvider: [`Bool of bool | `LinkedEditingRangeOptions
  | `LinkedEditingRangeRegistrationOptions] option;

	callHierarchyProvider: [`Bool of bool | `CallHierarchyOptions
  | `CallHierarchyRegistrationOptions] option;

	semanticTokensProvider: [`SemanticTokensOptions
    | `SemanticTokensRegistrationOptions] option;

  monikerProvider: [`Bool of bool | `MonikerOptions | `MonikerRegistrationOptions] option;

	typeHierarchyProvider: [`Bool of bool | `TypeHierarchyOptions
  | `TypeHierarchyRegistrationOptions] option;

	inlineValueProvider: [`Bool of bool | `InlineValueOptions
      | `InlineValueRegistrationOptions] option;

	inlayHintProvider: [`Bool of bool | `InlayHintOptions
    | `InlayHintRegistrationOptions] option;

    diagnosticProvider: [`DiagnosticOptions | `DiagnosticRegistrationOptions] option;

  workspaceSymbolProvider: [`Bool of bool | `WorkspaceSymbolOptions];

  workspace: workspace;*)
	experimental: lspAny option;
}

let initialized = ref false;;
   
type request = {
  processId: [`Null | `Int of int];
  clientInfo: client_info option;
  locale: string option;
  rootPath: [`Null | `String of string] option;
  rootUri: uri;
  initializationOptions: lspAny option;
  clientCapabilities: clientCapabilities;
  trace: traceValue option;
  workspaceFolders:  [`Null | `WorkspaceFolders of workspaceFolder list] option;
} 
and client_info = {
  name: string;
  version: string option 
};;

let json_to_pid : t -> [`Null | `Int of int] = function
  | (`Null | `Int _) as i -> i
  | json -> Type_error ("process_id is of wrong type", json) |> raise

let json_to_client_info = function
  | `Assoc _ as json -> {
      name = (json |> get_req_mem "name" |> to_string);
      version = (json |> get_opt_mem "version" |> to_string_opt);
    }
  | json -> Type_error ("client_info is of wrong type", json) |> raise

let opt_to_client_info : t option -> client_info option = function
  | None -> None
  | Some cli_Info -> Some (json_to_client_info cli_Info);;
  
let json_to_root_path : t -> [`Null | `String of string] = function  
  | (`Null | `String _) as i -> i
  | json -> Type_error ("rootpath is of wrong type", json) |> raise;;

let opt_to_root_path = function
  | None -> None
  | Some cli_Info -> Some (json_to_root_path cli_Info);;
  

let request_of_yojson json = 
  {
  processId = json |> get_req_mem "processId" |> json_to_pid;
  clientInfo = json |> get_opt_mem "client_info" |> opt_to_client_info;
  locale = json |> get_opt_mem "locale" |> to_string_opt;
  rootPath = json |> get_opt_mem "rootPath" |> opt_to_root_path;
  rootUri = json |> get_req_mem "rootUri" |> json_to_root_uri;
  initializationOptions = json |> get_opt_mem  "initializationOptions" |> opt_to_lsp_any;
  clientCapabilities = {l = true};
  trace = json |> get_opt_mem  "trace" |> opt_to_trace;
  workspaceFolders = json |> get_opt_mem "workspaceFolders" |> opt_to_workspace_folders
};;

type response = {
  capabilities: serverCapabilities;
  serverInfo: serverInfo option;
}
and serverInfo = {
  name: string;
  version: string option
};;

type error = {
  retry: bool;
}

let yojson_of_result res = 
  `Assoc ["capabilities", (optLspAny_to_json res.capabilities.experimental)];; 

let yojson_of_error_data (err : error) : Yojson.Basic.t  = 
  `Assoc ["retry", `Bool err.retry];;

let resp_to_json id resp : Yojson.Basic.t = 
  match resp with
  | Ok res -> yojson_of_result res |>
       Response.ok id  |> Response.yojson_of_t
  | Error err -> let open Response in  
    yojson_of_error_data err |> 
    Error.make Response.Error.Code.ServerNotInitialized "Server was already initialized bozo" |>
    Response.error id |> Response.yojson_of_t;;

let initialize process_id uri : (response, error) Result.t =
  let _ = uri in
  try
    assert (!initialized = false);
    initialized := true; 
    match process_id with
    | `Null -> Ok {capabilities = {experimental = Some Null}; serverInfo = None}
    | `Int _ -> Ok {capabilities = {experimental = Some Null}; serverInfo = None}
  with _ -> Error {retry = false}
    ;;

let respond params : Yojson.Basic.t =
  let open Response in
  let id = Id.t_of_yojson (`Int 7) in
  try
    let fields = request_of_yojson params in 
      initialize fields.processId fields.rootUri |> resp_to_json id 
  with
  | Missing_Member str -> yojson_of_error_data {retry = false} |> 
    Error.make Error.Code.InvalidRequest str |>
    Response.error id |> Response.yojson_of_t
  | str ->  yojson_of_error_data {retry = false} |> 
    Error.make Error.Code.InternalError (Printexc.to_string str) |>
    Response.error id |> Response.yojson_of_t

