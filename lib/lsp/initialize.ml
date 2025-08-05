open Yojson.Basic.Util
open Yojson.Basic
open Rpc

type uri = string;;

(*type workspace = {
		workspaceFolders: workspaceFoldersServerCapabilities option;
		fileOperations: fileOperations option
    }

and fileOperations = {
	didCreate: fileOperationRegistrationOptions option;

	willCreate: fileOperationRegistrationOptions option;

  didRename: fileOperationRegistrationOptions option;

	willRename: fileOperationRegistrationOptions option;

	didDelete: fileOperationRegistrationOptions option;

	willDelete: fileOperationRegistrationOptions option;
};;*)

let to_string_opt = function
  | None -> None
  | Some str -> Some (to_string str);;

    type lspAny = 
    | Null
    | String of string
    | LspObject of lspObject
    | LspArray of lspArray
    | Int of int
    | UInt of int
    | Decimal of float
    | Bool of bool

    and lspObject = (string * lspAny) list

    and lspArray = lspAny list;;
      
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

module Initialize = struct
 let initialized = ref false;;
   
  module Req = struct
    module Types = struct

    type client_info = {
      name: string;
      version: string option 
    };;
    
    type workspaceFolder = {
    uri: uri;
    name: string
  };;
    
    type request = {
      processId: [`Null | `Int of int];
      clientInfo: client_info option;
      locale: string option;
      rootPath: [`Null | `String of string] option;
      rootUri: uri;
      initializationOptions: lspAny option;
      clientCapabilities: clientCapabilities;
      trace: [`Off | `Messages | `Verbose] option;
      workspaceFolders:  [`Null | `WorkspaceFolders of workspaceFolder list] option;
    };; 

    end
    module Conversions = struct
      open Types

let json_to_p_id : t -> [`Null | `Int of int] = function
  | (`Null | `Int _) as i -> i
  | json -> raise (Type_error ("process_id is of wrong type", json))

  let json_to_client_info = function
  | `Assoc _ as json -> {
      name = (json |> get_req_mem "name" |> to_string);
      version = (json |> get_opt_mem "version" |> to_string_opt);
    }
  | json -> raise (Type_error ("client_info is of wrong type", json))

  let opt_to_client_info : t option -> client_info option = function
    | None -> None
    | Some cli_Info -> Some (json_to_client_info cli_Info);;
    
  let json_to_root_path : t -> [`Null | `String of string] = function  
    | (`Null | `String _) as i -> i
    | json -> raise (Type_error ("rootpath is of wrong type", json));;

  let opt_to_root_path = function
    | None -> None
    | Some cli_Info -> Some (json_to_root_path cli_Info);;
  
let json_to_root_uri : t -> uri = function
  | `String s -> s 
  | json -> raise (Type_error ("rootUri is of wrong type", json));;
 

  let rec json_to_lsp_any : t -> lspAny = function
  | `Null -> Null
  | `String str -> String str
  | `Assoc ass -> LspObject (List.map (fun obj -> let (str, json) = obj in (str, json_to_lsp_any json)) ass);
  | `List ls -> LspArray (List.map (fun ele -> json_to_lsp_any ele) ls)     
  | `Int i -> if i >= 0 then Int i else UInt i  
  | `Float f -> Decimal f 
  | `Bool b -> Bool b;;  

  let opt_to_lsp_any = function
    | None -> None
    | Some lspAny -> Some (json_to_lsp_any lspAny);;

  let rec lspAny_to_json : lspAny -> t = function
  | Null -> `Null
  | String str -> `String str
  | LspObject ass -> `Assoc (List.map (fun obj -> let (str, lsp) = obj in (str, lspAny_to_json lsp)) ass)
  | LspArray ls -> `List (List.map (fun ele -> lspAny_to_json ele) ls)     
  | (Int i | UInt i) -> `Int i  
  | Decimal f -> `Float f 
  | Bool b -> `Bool b;;  
  
  let optLspAny_to_json : lspAny option -> t = function
  | None -> `Null
  | Some ele -> lspAny_to_json ele;;
 
  let str_to_trace = function 
    | "off" -> `Off
    | "messages" -> `Messages
    | "verbose" -> `Verbose
    | x -> raise (Type_error ("trace value does not match any known string", `String x));;

  let json_to_trace = function 
  | `String str -> str_to_trace str
  | json -> raise (Type_error ("trace value is of wrong type", json));;
  
  let opt_to_trace = function
    | None -> None
    | Some trace -> Some (json_to_trace trace);;
  
  let json_workspace_folder : t -> workspaceFolder = function
  | `Assoc _ as json -> {
      uri = (json |> get_req_mem "uri" |> to_string);
      name = (json |> get_req_mem "name" |> to_string);
    }
  | json -> raise (Type_error ("workspaceFolder is of wrong type", json));;

  let json_to_workspace_folders : t -> [`Null | `WorkspaceFolders of workspaceFolder list] = function
  | `Null ->  `Null
  | `List ls ->  `WorkspaceFolders (List.map (fun wf -> json_workspace_folder wf) ls)
  | json -> raise (Type_error ("WorkspaceFolders is of wrong type", json))
  
  let opt_to_workspace_folders : t option -> [`Null | `WorkspaceFolders of workspaceFolder list] option = function
  | None -> None
  | Some wf -> Some (json_to_workspace_folders wf);;
 (*Json to Type*)


  let request_of_yojson json = 
    {
    processId = json |> get_req_mem "process_id" |> json_to_p_id;
    clientInfo = json |> get_opt_mem "client_info" |> opt_to_client_info;
    locale = json |> get_opt_mem "locale" |> to_string_opt;
    rootPath = json |> get_opt_mem "rootPath" |> opt_to_root_path;
    rootUri = json |> get_req_mem "rootUri" |> json_to_root_uri;
    initializationOptions = json |> get_opt_mem  "initializationOptions" |> opt_to_lsp_any;
    clientCapabilities = {l = true};
    trace = json |> get_opt_mem  "trace" |> opt_to_trace;
    workspaceFolders = json |> get_opt_mem "workspaceFolders" |> opt_to_workspace_folders
    };;

  end
  end

  open Req.Types
  open Req.Conversions

    module Resp = struct 

    type serverInfo = {
      name: string;
      version: string option
    }
      type result = {
      capabilities: serverCapabilities;
      serverInfo: serverInfo option;
    };;

    let yojson_of_result res : Yojson.Basic.t = 
      `Assoc ["capabilities", (optLspAny_to_json res.capabilities.experimental)];;
    

    type error = {
      retry: bool;
    }

    type response = (result, error) Result.t;;

    let yojson_of_error err : Yojson.Basic.t  = 
      `Assoc ["retry", `Bool err.retry];;

    let yojson_of_response : response -> t = function
      | Ok res -> yojson_of_result res
      | Error err -> yojson_of_error err

    end

    open Resp
    
    let initialize process_id uri : response =
      let _ = uri in
      try
        assert (!initialized = false);
        initialized := true; 
        match process_id with
        | `Null -> Ok {capabilities = {experimental = Some Null}; serverInfo = None}
        | `Int _ -> Ok {capabilities = {experimental = Some Null}; serverInfo = None}
      with _ -> Error {retry = false}
        ;;

    let choose_between id resp = 
      let open Response in
      match resp with
      | Ok res -> yojson_of_result res |> (fun res -> Ok res) |> construct_response id |> Response.yojson_of_t
      | Error err -> yojson_of_error err |> Error.construct_error Error.Code.ServerNotInitialized "Server was already initialized bozo" |>
          Response.construct_response id |> Response.yojson_of_t;;

    let respond params : Yojson.Basic.t =
      let open Response.Error in
    try
        let id = Id.t_of_yojson (`Int 7) in
        let fields = request_of_yojson params in 
          initialize fields.processId fields.rootUri |> choose_between id 
    with
    | Missing_Member str -> yojson_of_error {retry = false} |> 
      construct_error Code.InvalidRequest str |>
          Response.construct_response (`Int 0) |> Response.yojson_of_t
    | str ->  yojson_of_error {retry = false} |> 
      construct_error Code.InternalError (Printexc.to_string str) |>
          Response.construct_response (`Int 0) |> Response.yojson_of_t

end
