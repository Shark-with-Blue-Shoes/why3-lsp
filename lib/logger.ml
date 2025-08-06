open Printf

let oc = open_out "why3-lsp.log";;

let log_req cnt_len str bytes_read = 
  let time = Unix.time () |> Unix.localtime in
  (sprintf "[INPUT] [TIME: %d:%d:%d] [BYTES READ: %d] [CONTENT-LENGTH: %d] DATA: %s\n"
      cnt_len time.tm_hour time.tm_min time.tm_sec bytes_read str) |>
      output_string oc;
    flush oc;;

