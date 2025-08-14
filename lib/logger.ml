open Printf

let oc = open_out "why3-lsp.log";;

let log_in cnt_len str bytes_read = 
  let time = Unix.time () |> Unix.localtime in
  (sprintf "[INPUT] [TIME: %d:%d:%d] [BYTES READ: %d] [CONTENT-LENGTH: %d] DATA: %s\n"
      time.tm_hour time.tm_min time.tm_sec bytes_read cnt_len str) |>
      output_string oc;
    flush oc;;

let log_out str = 
  let time = Unix.time () |> Unix.localtime in
  (sprintf "[OUTPUT] [TIME: %d:%d:%d] DATA: %s\n"
      time.tm_hour time.tm_min time.tm_sec str) |>
      output_string oc;
    flush oc;;
