(* Abstract LLM provider interface *)
type reply =
  | Content of string
  | ToolCalls of Yojson.Safe.t list
  | Malformed of string

type t = { name : string; send_request : Message.message list -> reply Lwt.t }

(* Convert messages to request format for the provider *)
let messages_to_request messages provider = provider.send_request messages
