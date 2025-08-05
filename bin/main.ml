open Why3_lsp.Runtime

let rec loop oc =
  let str = read_line () in
  let time = Unix.time () |> Unix.localtime in
  (Printf.sprintf "[INPUT] [TIME: %d:%d:%d] DATA: %s\n%!" time.tm_hour time.tm_min time.tm_sec str) |>
  output_string oc;
  flush oc;
  interp str; 
  loop oc;;

let () = let oc = open_out "example.txt" in
loop oc;;

