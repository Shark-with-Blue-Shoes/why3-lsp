open Yojson
open In_channel

let () =
  let chan = open_text "book.json" in
  let buf = input_all chan in
  let json1 = Basic.from_string buf in
  let json2 = Basic.from_file "book.json" in
  print_endline (if Yojson.Basic.equal json1 json2 then "OK" else "FAIL")
