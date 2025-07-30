open Why3.Itp_communication
open Why3
open Scheduler
open Printf


module Lsp_Protocol = struct

  let list_requests: ide_request list ref = ref []

  let get_requests () =
    if List.length !list_requests > 0 then
    let l = List.rev !list_requests in
    list_requests := [];
      l else [];;

  let send_request r =
    list_requests := r :: !list_requests

  let notification_list: notification list ref = ref []

  let notify n =
    notification_list := n :: !notification_list

  let get_notified () : Why3.Itp_communication.notification list =
    if List.length !notification_list > 0 then
    let l = List.rev !notification_list in
    notification_list := [];
      l else [];;
end


let get_notified = Lsp_Protocol.get_notified

let send_request = Lsp_Protocol.send_request

module Server = Itp_server.Make (Unix_scheduler) (Lsp_Protocol)


let treat_notification fmt n =
  let _ = fmt in
  match n with
  | Reset_whole_tree                        -> printf "Reset whole tree\n%!" 
  | _ -> printf "What the hell are you doing!\n%!"
