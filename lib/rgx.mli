exception Rgx_failure of string
val get_header : string -> string
val get_json : string -> string
val split_header_json : string -> string * string
val get_content_len : string -> int
val get_content_typ : string -> string
