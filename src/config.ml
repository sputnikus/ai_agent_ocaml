let get_env name ~default =
  match Sys.getenv_opt name with
  | Some value -> value
  | None -> default

let openai_api_key () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | Some key -> key
  | None ->
      failwith "Missing OPENAI_API_KEY environment variable"

let agent_name () = get_env "AGENT_NAME" ~default:"OCamlAgent"

let debug_enabled () = Sys.getenv_opt "AGENT_DEBUG" = Some "1"

let model () = get_env "OPENAI_MODEL" ~default:"gpt-3.5-turbo-0125"

let tool_timeout () =
  match Sys.getenv_opt "AGENT_TOOL_TIMEOUT" with
  | Some s -> int_of_string_opt s |> Option.value ~default:10
  | None -> 10

let log_file () = Sys.getenv_opt "AGENT_LOG"
