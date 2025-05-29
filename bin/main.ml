let () =
  Lwt_main.run (
  let uri = Uri.of_string "https://api.openai.com/v1/chat/completions" in
  let api_key = Sys.getenv "OPENAI_API_KEY" in
  let headers =
    Cohttp.Header.init_with "Authorization" ("Bearer " ^ api_key)
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  in

  let body_json =
    `Assoc [
      "model", `String "gpt-3.5-turbo-0125";
      "messages", `List [
        `Assoc [ "role", `String "system"; "content", `String "You are a helpful assistant." ];
        `Assoc [ "role", `String "user"; "content", `String "Hello! What's the capital of France?" ]
      ]
    ]
  in

  let body = Yojson.Safe.to_string body_json in

  let%lwt _, body_stream =
    Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) uri
  in

  let%lwt body_str = Cohttp_lwt.Body.to_string body_stream in
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
                  | Some (`String reply) ->
                      Lwt.return(Printf.printf "\nAssistant: %s\n" reply)
                  | _ -> Lwt.return(Printf.eprintf "Could not extract assistant content.\n")
                )
              | _ -> Lwt.return(Printf.eprintf "Missing 'message' field.\n")
            )
          | _ -> Lwt.return(Printf.eprintf "First choice is not an object.\n")
        )
      | _ -> Lwt.return(Printf.eprintf "Missing or invalid 'choices' field.\n")
    )
  | _ -> Lwt.return(Printf.eprintf "Invalid top-level JSON format.\n")
)
