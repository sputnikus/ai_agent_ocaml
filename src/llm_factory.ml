open Llm_provider

let create_provider () =
  match String.lowercase_ascii (Config.llm_provider ()) with
  | "openai" -> Openai_provider.create ()
  | "anthropic" -> Anthropic_provider.create ()
  | provider -> 
      failwith ("Unsupported LLM provider: " ^ provider ^ ". Supported providers: openai, anthropic")

let get_provider () = 
  let provider = create_provider () in
  Logger.info ("Using LLM provider: " ^ provider.name);
  provider