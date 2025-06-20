open Llm_provider

let extract_reply body_str =
  Logger.debug ~tag:"anthropic_response" body_str;
  let json = Yojson.Safe.from_string body_str in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "content" fields with
      | Some (`List (first_content :: _)) -> (
          match first_content with
          | `Assoc content_fields -> (
              match List.assoc_opt "text" content_fields with
              | Some (`String reply) -> Some reply
              | _ -> Some "[Could not extract assistant text content.]")
          | _ -> Some "[First content is not an object.]")
      | _ -> Some "[Missing or invalid 'content' field.]")
  | _ -> None

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
  match extract_reply body_str with
  | Some reply -> Lwt.return (Content reply)
  | None -> Lwt.return (Malformed "[error parsing Anthropic reply]")

let create () = { name = "Anthropic"; send_request }
