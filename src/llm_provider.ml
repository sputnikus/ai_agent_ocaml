(* Abstract LLM provider interface *)

type t = { name : string; send_request : Message.message list -> string Lwt.t }

(* Convert messages to request format for the provider *)
let messages_to_request messages provider = provider.send_request messages
