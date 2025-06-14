open Llm_provider

let provider = lazy (Llm_factory.get_provider ())

let fetch_reply_messages messages =
  let provider = Lazy.force provider in
  provider.send_request messages

(* Deprecated: kept for backwards compatibility *)
let fetch_reply body_json =
  (* Convert old JSON body format to Message.message list *)
  let messages =
    match body_json with
    | `Assoc fields -> (
        match List.assoc_opt "messages" fields with
        | Some (`List msg_list) ->
            List.filter_map
              (fun msg ->
                match msg with
                | `Assoc msg_fields -> (
                    let role_opt = List.assoc_opt "role" msg_fields in
                    let content_opt = List.assoc_opt "content" msg_fields in
                    match (role_opt, content_opt) with
                    | Some (`String "system"), Some (`String content) ->
                        Some { Message.role = `System; content }
                    | Some (`String "user"), Some (`String content) ->
                        Some { Message.role = `User; content }
                    | Some (`String "assistant"), Some (`String content) ->
                        Some { Message.role = `Assistant; content }
                    | _ -> None)
                | _ -> None)
              msg_list
        | _ -> [])
    | _ -> []
  in
  fetch_reply_messages messages
