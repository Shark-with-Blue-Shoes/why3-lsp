exception Rgx_failure of string

open Printf

let get_header str = 
  try 
  let headers_rgx = "(.*)\\\\r\\\\n" in
    let rgx = Re.Perl.compile_pat headers_rgx in 
      let mtch = Re.exec rgx str in
        let headers = Re.Group.get mtch 1 in headers
  with
    e -> Rgx_failure (sprintf "Error in getting headers: %s\n" (Printexc.to_string e)) |> raise;;

let get_json str = 
  try 
    let arr_rgx = "\\[.*\\]" in
    let rgx = Re.Perl.compile_pat arr_rgx  in
      let mtch = Re.exec rgx str in 
        let json = Re.Group.get mtch 0 in json
  with _ -> try
    let obj_rgx = "\\{.*\\}" in
      let rgx = Re.Perl.compile_pat obj_rgx in 
        let mtch = Re.exec rgx str in 
          let json = Re.Group.get mtch 0 in json
  with 
    e -> Rgx_failure (sprintf "Error in getting json: %s\n" (Printexc.to_string e)) |> raise;;

let split_header_json str = 
    let json = get_json str in
      let headers = get_header str in
              (headers, json)

let get_content_len str = 
  try
    let rgx = Re.Perl.compile_pat "Content-Length: (\\d+)" in
      let mtch = Re.exec rgx str in
        let content_len_unsan = Re.Group.get mtch 1 in 
        let content_len = (int_of_string content_len_unsan) in content_len
  with 
  | e -> Rgx_failure (sprintf "Error in getting content length: %s\n" (Printexc.to_string e)) |> raise;;


let get_content_typ str = 
  try
    let rgx = Re.Perl.compile_pat "Content-Type: \"(.*)\"\\\\r\\\\n" in 
      let mtch = Re.exec rgx str in 
        let content_type = Re.Group.get mtch 1 in content_type
  with 
  | e -> Rgx_failure (sprintf "Error in getting content type: %s\n" (Printexc.to_string e)) |> raise;;

