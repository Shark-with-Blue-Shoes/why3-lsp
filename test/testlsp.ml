open Why3_lsp.Rpc

let nums = [54; -1; 65; 71; 27901; 186972806905; 2; 19619629265; 0; 275205729373; 0; 2752027075025]

let make_json_object (num_value : int) : string =
  Printf.sprintf "{ \"version\": \"2.0\", \"num\": %d }" num_value

let cases = List.map (fun n -> make_json_object n) nums 

let () =
  let p = Lwt_list.iter_s (fun str -> Lwt.return (interp str)) cases in
    Lwt_main.run p;
