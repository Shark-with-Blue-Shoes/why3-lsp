open Why3_lsp
open Rgx
open Printf

let get_content_length str = 
  try
    let rgx = Re.Perl.compile_pat "\\d+" in
      let mtch = Re.exec rgx str in
        let content_len_unsan = Re.Group.get mtch 0 in 
        let content_len = (int_of_string content_len_unsan) in content_len
  with 
  | e -> raise (Rgx_failure (sprintf "Error in getting content length: %s\n" (Printexc.to_string e)));;

let rec loop oc =
  let str = read_line () in
    let cnt_len = (get_content_length str) + 2 in
      let byt = Bytes.create cnt_len in
        let bytes_read = In_channel.input stdin byt 0 cnt_len in
        let str_of_bytes = Bytes.to_string byt in
        let time = Unix.time () |> Unix.localtime in
        (sprintf "[INPUT] [TIME: %d:%d:%d] [BYTES READ: %d] DATA: %s\n%!"
        time.tm_hour time.tm_min time.tm_sec bytes_read str_of_bytes) |>
        output_string oc;
      flush oc;
    loop oc;;

let () = let oc = open_out "example.txt" in
loop oc;;

