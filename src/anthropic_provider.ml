open Llm_provider

let extract_reply body_str =
  Logger.debug ~tag:"anthropic_response" body_str;
  let json = Yojson.Safe.from_string body_str in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "content" fields with
      | Some (`List content_blocks) ->
          let tool_calls = ref [] in
          let text_parts = ref [] in

          (* Process each content block *)
          List.iter
            (fun block ->
              match block with
              | `Assoc block_fields -> (
                  match List.assoc_opt "type" block_fields with
                  | Some (`String "text") -> (
                      match List.assoc_opt "text" block_fields with
                      | Some (`String text) -> text_parts := text :: !text_parts
                      | _ -> ())
                  | Some (`String "tool_use") -> (
                      (* Extract tool call information *)
                      let id =
                        List.assoc_opt "id" block_fields |> function
                        | Some (`String id) -> Some id
                        | _ -> None
                      in
                      let name =
                        List.assoc_opt "name" block_fields |> function
                        | Some (`String name) -> Some name
                        | _ -> None
                      in
                      let input =
                        List.assoc_opt "input" block_fields |> function
                        | Some input -> Some input
                        | _ -> None
                      in

                      match (name, input) with
                      | Some name, Some input ->
                          let tool_call =
                            `Assoc
                              [
                                ( "id",
                                  match id with
                                  | Some id -> `String id
                                  | None -> `Null );
                                ( "function",
                                  `Assoc
                                    [
                                      ("name", `String name);
                                      ( "arguments",
                                        `String (Yojson.Safe.to_string input) );
                                    ] );
                              ]
                          in
                          tool_calls := tool_call :: !tool_calls
                      | _ -> ())
                  | _ -> ())
              | _ -> ())
            content_blocks;

          (* Return based on what we found *)
          if !tool_calls <> [] && !text_parts <> [] then
            (* Has both text and tool calls - return mixed content *)
            MixedContent
              (String.concat " " (List.rev !text_parts), List.rev !tool_calls)
          else if !tool_calls <> [] then
            (* Has only tool calls - return them *)
            ToolCalls (List.rev !tool_calls)
          else if !text_parts <> [] then
            (* Has only text content - return it *)
            Content (String.concat " " (List.rev !text_parts))
          else
            (* No content found *)
            Malformed "[No valid content found in response]"
      | _ -> Malformed "[Missing or invalid 'content' field.]")
  | _ -> Malformed "[Invalid JSON response format]"

let message_to_json msg = Message.yojson_of_message msg

(* Anthropic API requires system message to be separate from messages array *)
let separate_system_message messages =
  let _, other_msgs =
    List.partition (fun msg -> msg.Message.role = `System) messages
  in
  other_msgs

let build_request_body messages =
  let non_system_msgs = separate_system_message messages in
  let base_fields =
    [
      ("model", `String (Config.anthropic_model ()));
      ("messages", `List (List.map message_to_json non_system_msgs));
      ("max_tokens", `Int (Config.max_chars ()));
      ("temperature", `Float 0.7);
      (* new: LLM-API-visible tools *)
      ("tools", Tool.all_to_json ());
    ]
  in
  let fields = ("system", `String system_prompt) :: base_fields in
  `Assoc fields

let send_request messages =
  let uri = Uri.of_string "https://api.anthropic.com/v1/messages" in
  let api_key = Config.anthropic_api_key () in
  let headers =
    Cohttp.Header.init_with "x-api-key" api_key |> fun h ->
    Cohttp.Header.add h "Content-Type" "application/json" |> fun h ->
    Cohttp.Header.add h "anthropic-version" "2023-06-01"
  in
  let body_json = build_request_body messages in
  Logger.debug ~tag:"anthropic_request" (Yojson.Safe.pretty_to_string body_json);
  let body = Yojson.Safe.to_string body_json in

  let%lwt _, body_stream =
    Cohttp_lwt_unix.Client.post ~headers
      ~body:(Cohttp_lwt.Body.of_string body)
      uri
  in
  let%lwt body_str = Cohttp_lwt.Body.to_string body_stream in
  Lwt.return (extract_reply body_str)

let create () = { name = "Anthropic"; send_request }
