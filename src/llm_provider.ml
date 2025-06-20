(* Abstract LLM provider interface *)
type reply =
  | Content of string
  | ToolCalls of Yojson.Safe.t list
  | Malformed of string

type t = { name : string; send_request : Message.message list -> reply Lwt.t }

(* Convert messages to request format for the provider *)
let messages_to_request messages provider = provider.send_request messages

(* System prompt for models *)
let system_prompt =
  {|You are a helpful AI assistant that can engage in natural conversation and use tools when needed.

  RESPONSE GUIDELINES:

  1. For normal conversation, respond in natural language
  2. When you need to use tools, respond ONLY with a JSON object in this exact format:
     {"tool_calls":[{"name":"tool_name","arguments":tool_args}]}

  Examples:

  Conversation:
  User: "Hi, how are you?"
  Assistant: "Hello! I'm doing well, thank you for asking. How can I help you today?"

  Tool Usage:
  User: "What's 2 plus 2?"
  Assistant: {"tool_calls":[{"name":"math","arguments":{"expression":"2+2"}}]}

  Multiple Tools:
  User: "What time is it and calculate 4*5"
  Assistant: {"tool_calls":[{"name":"time","arguments":null},{"name":"math","arguments":{"expression":"4*5"}}]}

  IMPORTANT:
  - Only use JSON format when calling tools
  - For all other responses, use natural language
  - After using tools, I will follow up with a natural language response
  |}
