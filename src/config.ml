(* REPL colors *)
let color_reset = "\027[0m"
let blue = "\027[34m"
let green = "\027[32m"
let yellow = "\027[33m"
let magenta = "\027[35m"
let red = "\027[31m"

let get_env name ~default =
  match Sys.getenv_opt name with Some value -> value | None -> default

let openai_api_key () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | Some key -> key
  | None -> failwith "Missing OPENAI_API_KEY environment variable"

let anthropic_api_key () =
  match Sys.getenv_opt "ANTHROPIC_API_KEY" with
  | Some key -> key
  | None -> failwith "Missing ANTHROPIC_API_KEY environment variable"

let agent_name () = get_env "AGENT_NAME" ~default:"OCamlAgent"
let debug () = Sys.getenv_opt "AGENT_DEBUG" = Some "1"

(* Provider selection *)
let llm_provider () = get_env "LLM_PROVIDER" ~default:"openai"

(* Model configurations *)
let openai_model () = get_env "OPENAI_MODEL" ~default:"gpt-4.1"

let anthropic_model () =
  get_env "ANTHROPIC_MODEL" ~default:"claude-3-haiku-20240307"

let tool_timeout () =
  match Sys.getenv_opt "AGENT_TOOL_TIMEOUT" with
  | Some s -> int_of_string_opt s |> Option.value ~default:10
  | None -> 10

let log_file () = Sys.getenv_opt "AGENT_LOG"

let max_chars () =
  match Sys.getenv_opt "AGENT_MAX_CHARS" with
  | Some s -> int_of_string_opt s |> Option.value ~default:8000
  | None -> 8000
