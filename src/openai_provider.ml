open Llm_provider

let extract_reply body_str =
  Logger.debug ~tag:"openai_response" body_str;
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string body_str in
    let message = json |> member "choices" |> index 0 |> member "message" in
    match member "tool_calls" message with
    | `List calls -> ToolCalls calls
    | _ -> (
        match member "content" message with
        | `String s -> Content s
        | _ -> Malformed "[Missing or invalid content field]")
  with exn -> Malformed (Printexc.to_string exn)

let message_to_json msg = Message.yojson_of_message msg

let build_request_body messages =
  `Assoc
    [
      ("model", `String (Config.openai_model ()));
      ("messages", `List (List.map message_to_json messages));
      (* new: LLM-API-visible tools *)
      ("tools", Tool.all_to_json ());
    ]

let send_request messages =
  let uri = Uri.of_string "https://api.openai.com/v1/chat/completions" in
  let api_key = Config.openai_api_key () in
  let headers =
    Cohttp.Header.init_with "Authorization" ("Bearer " ^ api_key) |> fun h ->
    Cohttp.Header.add h "Content-Type" "application/json"
  in
  let body_json = build_request_body messages in
  Logger.debug ~tag:"openai_request" (Yojson.Safe.pretty_to_string body_json);
  let body = Yojson.Safe.to_string body_json in

  let%lwt _, body_stream =
    Cohttp_lwt_unix.Client.post ~headers
      ~body:(Cohttp_lwt.Body.of_string body)
      uri
  in
  let%lwt body_str = Cohttp_lwt.Body.to_string body_stream in
  Lwt.return (extract_reply body_str)

let create () = { name = "OpenAI"; send_request }
