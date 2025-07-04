open Yojson
open Yojson.Basic.Util
open In_channel
open Printf

module Id = struct
  type t =
    [ `String of string
    | `Int of int
    ]

  let yojson_of_t = function
    | `String s -> `String s
    | `Int i -> `Int i
  ;;

  let t_of_yojson = function
    | `String s -> `String s
    | `Int i -> `Int i
  ;;

  let hash x = Hashtbl.hash x
  let equal = ( = )
end

module Constant = struct
  let jsonrpc = "jsonrpc"
  let jsonrpcv = "2.0"
  let id = "id"
  let method_ = "method"
  let params = "params"
  let result = "result"
  let error = "error"
end

let assert_jsonrpc_version json =

  let jsonrpc = json |> member "version" |> to_string in
  if not (String.equal jsonrpc Constant.jsonrpcv)
  then
    raise (Json_error ("invalid packet: jsonrpc version doesn't match " ^ jsonrpc))
;;

let () =
  let chan = open_text "book.json" in
  let buf = input_all chan in
  let json = Basic.from_string buf in
  (* Locally open the JSON manipulation functions *)
  try
    assert_jsonrpc_version json;
    let title = json |> member "title" |> to_string in
    let tags = json |> member "tags" |> to_list |> filter_string in
    let pages = json |> member "pages" |> to_int in
    let is_online = json |> member "is_online" |> to_bool_option in
    let is_translated = json |> member "is_translated" |> to_bool_option in
    let authors = json |> member "authors" |> to_list in
    let names = List.map (fun json -> member "name" json |> to_string) authors  in
    (* Print the results of the parsing *)
    printf "Title: %s (%d)\n" title pages;
    printf "Authors: %s\n" (String.concat ", " names);
    printf "Tags: %s\n" (String.concat ", " tags);
    let string_of_bool_option =
      function
      | None -> "<unknown>"
      | Some true -> "yes"
      | Some false -> "no" in
    printf "Online: %s\n" (string_of_bool_option is_online);
    printf "Translated: %s\n" (string_of_bool_option is_translated)
  with
    | Yojson__Basic.Util.Type_error (x, p) -> printf "Type error: %s\n" x
    | Json_error err -> printf "Does not fulfill JSON RPC 2.0 protocol: %s" err
    | _ -> print_string "strange error"
    
