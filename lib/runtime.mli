
(*A map of the "name": function for requests*)
val all_request_calls :
  (Yojson.Basic.t -> Yojson.Basic.t) Rpc.StringMap.t

(*Checks all request calls for the proper function and runs the function*)
val call_request : Rpc.Request.t -> Yojson.Basic.t

(*A map of the "name": function for notifs*)
val all_notification_calls : 'a Rpc.StringMap.t

(*Checks all notification calls for the proper function and runs the function*)
val call_notification : Rpc.Notification.t -> unit

(*This responds to a batch call*)
val respond_to_batch : Rpc.Packet.call -> unit

(*interp cnt_len cnt_typ buf: It turns the params into a packet and tries to respond*)
val interp : int -> string -> string -> unit

(*Reads a cnt_len amount of bytes from the input, returns a string representation
of the bytes and the amount read*)
val get_body : int -> string * int

(*The main runtime*)
val loop : unit -> 'a
