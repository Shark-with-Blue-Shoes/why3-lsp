open Yojson.Basic.Util
open Yojson.Basic
open Rpc

  type uri =
    [ `String of string | `Null ];;

let rooturi_of_json : t -> uri = function
  | `String s -> `String s 
  | `Null -> `Null
  | json -> Type_error ("rootUri is of wrong type", json) |> raise;;
 
type workspaceFolder = {
  uri: uri;
  name: string
};;
    
let opt_to_workspace_folders opt : [`Null | `WorkspaceFolders of workspaceFolder list] option =

  let json_workspace_folder : t -> workspaceFolder = function
    | `Assoc _ as json -> {
      uri = (json |> get_req_mem "uri" |> rooturi_of_json);
      name = (json |> get_req_mem "name" |> to_string );
      }
    | json -> Type_error ("workspaceFolder is of wrong type", json) |> raise in

  let json_to_workspace_folders : t -> [`Null | `WorkspaceFolders of workspaceFolder list] = function
    | `Null ->  `Null
    | `List ls ->  `WorkspaceFolders (List.map (fun wf -> json_workspace_folder wf) ls)
    | json -> Type_error ("WorkspaceFolders is of wrong type", json) |> raise in
  
  match opt with 
  | None -> None
  | Some wf -> Some (json_to_workspace_folders wf);;

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
      
let lspAny_of_json_opt opt =

  let rec json_to_lspAny : t -> lspAny = function
    | `Null -> Null
    | `String str -> String str
    | `Assoc ass -> LspObject (List.map (fun obj -> let (str, json) = obj in (str, json_to_lspAny json)) ass);
    | `List ls -> LspArray (List.map (fun ele -> json_to_lspAny ele) ls)     
    | `Int i -> if i >= 0 then Int i else UInt i  
    | `Float f -> Decimal f 
    | `Bool b -> Bool b in

  match opt with
  | None -> None
  | Some json -> Some (json_to_lspAny json);;

let json_of_lspAny_opt opt : t = 

  let rec lspAny_to_json : lspAny -> t = function
  | Null -> `Null
  | String str -> `String str
  | LspObject ass -> `Assoc (List.map (fun obj -> let (str, lsp) = obj in (str, lspAny_to_json lsp)) ass)
  | LspArray ls -> `List (List.map (fun ele -> lspAny_to_json ele) ls)     
  | (Int i | UInt i) -> `Int i  
  | Decimal f -> `Float f 
  | Bool b -> `Bool b in 

  match opt with
  | None -> `Null
  | Some ele -> lspAny_to_json ele;;
 
type traceValue = 
  | Off
  | Messages
  | Verbose

let opt_to_trace opt = 
  let str_to_trace = function 
    | "off" -> Off
    | "messages" -> Messages
    | "verbose" -> Verbose
    | x -> raise (Type_error ("trace value does not match any known string", `String x)) in
  let json_to_trace = function 
    | `String str -> str_to_trace str
    | json -> raise (Type_error ("trace value is of wrong type", json)) in

  match opt with
  | None -> None
  | Some trace -> Some (json_to_trace trace);;

