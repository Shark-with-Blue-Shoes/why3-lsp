exception Rgx_failure of string

open Printf

let get_content_len str = 
  try
    let rgx = Re.Perl.compile_pat "Content-Length: (\\d+)" in
      let mtch = Re.exec rgx str in
        let content_len_unsan = Re.Group.get mtch 1 in 
        let content_len = (int_of_string content_len_unsan) in content_len
  with 
  | e -> Rgx_failure (Printexc.to_string e |> sprintf "Error in getting content length: %s\n") |> raise;;

