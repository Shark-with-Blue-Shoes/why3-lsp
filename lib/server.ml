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
  | Reset_whole_tree                        -> printf "Reset_whole_tree\n%!"
  | Node_change _                  -> printf "Node_change\n%!"
  | New_node _ -> printf "New node\n%!"
  | Remove _                              -> printf "Remove\n%!"
  | Ident_notif_loc _loc                    -> printf "Ident_notif_loc"
  | Initialized _g_info                     -> printf "Initialized"
      (* TODO *)
  | Saved                                   ->  printf "Saved"
  | Saving_needed _b                        -> printf "Saving_needed"
  | Message _                             -> printf "Message"
  | Dead _                                  -> printf "Dead";
      (* This exception is matched in Unix_Scheduler *)
      raise Exit
  | File_contents (f, s, _, _)              -> printf "File %s is:\n%s" f s (* TODO print this correctly *)
  | Source_and_ce _                         -> printf "Source_and_ce"
  | Next_Unproven_Node_Id _                 -> printf "Next_Unproven_Node_Id"
  | Task _           -> printf "Task";
    (* coloring the source is useless in shell *)
    (*try
      let node = Hnode.find nodes id in
      node.node_task <- Some s;
      if id = !cur_id then print_goal fmt !cur_id
    with
      Not_found -> fprintf fmt "Could not find node %d@." id*)
