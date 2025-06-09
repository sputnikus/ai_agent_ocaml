open Tool

let system_prompt_with_tools =
  let descriptions = tools |> List.map tool_description |> String.concat "\n" in
  "You are a helpful AI assistant. When you want to use a tool, respond only \
   with a JSON object. Schema \n\
   {\"tool_call\": {\"name\": \"tool_name\", \"arguments\": \"Arguments JSON\"}}\n\
   Available tools:\n" ^ descriptions

(* Parse a tool call from LLM output *)
let try_parse_tool_call json_str =
  try
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string json_str in
    let call = json |> member "tool_call" in
    let name = call |> member "name" |> to_string in
    let args = call |> member "arguments" in
    (* We always want to return to handle and log in the REPL loop *)
    Some (name, args)
  with _ -> None
