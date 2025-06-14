open Llm_provider

let extract_reply body_str =
  Logger.debug ~tag:"openai_response" body_str;
  let json = Yojson.Safe.from_string body_str in
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "choices" fields with
      | Some (`List (first_choice :: _)) -> (
          match first_choice with
          | `Assoc choice_fields -> (
              match List.assoc_opt "message" choice_fields with
              | Some (`Assoc message_fields) -> (
                  match List.assoc_opt "content" message_fields with
                  | Some (`String reply) -> Some reply
                  | _ -> Some "[Could not extract assistant content.]")
              | _ -> Some "[Missing 'message' field.]")
          | _ -> Some "[First choice is not an object.]")
      | _ -> Some "[Missing or invalid 'choices' field.]")
  | _ -> None

let message_to_json msg = Message.yojson_of_message msg

let build_request_body messages =
  `Assoc
    [
      ("model", `String (Config.openai_model ()));
      ("messages", `List (List.map message_to_json messages));
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
  match extract_reply body_str with
  | Some reply -> Lwt.return reply
  | None -> Lwt.return "[error parsing OpenAI reply]"

let create () = { name = "OpenAI"; send_request }
