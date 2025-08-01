open Rpc_lib.Basic


module Lsp_Protocol = struct

  let list_requests: Request.t list ref = ref []

  let get_requests () : Request.t list =
    if List.length !list_requests > 0 then
    let l = List.rev !list_requests in
    list_requests := [];
      l else [];;

  let send_request r =
    list_requests := r :: !list_requests

  let notification_list: Notification.t list ref = ref []

  let send_notif n =
    notification_list := n :: !notification_list

  let get_notified () : Notification.t list =
    if List.length !notification_list > 0 then
    let l = List.rev !notification_list in
    notification_list := [];
      l else [];;
end
