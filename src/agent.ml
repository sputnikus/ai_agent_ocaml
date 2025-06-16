open Tool
open Config

type tool_call = { name : string; arguments : Yojson.Safe.t }

let system_prompt_with_tools =
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

let parse_tool_calls calls =
  try
    let open Yojson.Safe.Util in
    let parsed =
      List.filter_map
        (fun call ->
          try
            let func = call |> member "function" in
            let name = func |> member "name" |> to_string in
            let args_str = func |> member "arguments" |> to_string in
            let args = Yojson.Safe.from_string args_str in
            Some { name; arguments = args }
          with _ -> None)
        calls
    in
    Some parsed
  with _ -> None

let tool_call_to_string call =
  "{name:" ^ call.name ^ ",arguments:"
  ^ Yojson.Safe.to_string call.arguments
  ^ "}"

let run_tool_chain tool_calls =
  let rec loop_chain acc_outputs = function
    | [] -> Lwt.return (List.rev acc_outputs)
    | { name; arguments } :: rest -> (
        match find_tool name with
        | Some tool ->
            let args = Yojson.Safe.to_string arguments in
            Logger.info ~tag:"tool_call" (Printf.sprintf "%s %s" name args);
            let%lwt () =
              Lwt_io.printf "%sTool%s: %s(%s)\n\n" yellow color_reset name args
            in
            let start_time = Unix.gettimeofday () in
            let%lwt output =
              try%lwt tool.run arguments
              with exn ->
                let exception_message = Printexc.to_string exn in
                Logger.errorf ~tag:"tool_call" "[tool fail] %s %s: %s" name args
                  exception_message;
                Lwt.return ("[Tool failed]" ^ name ^ ": " ^ exception_message)
            in
            let duration = Unix.gettimeofday () -. start_time in
            Logger.resultf ~tag:"tool_call" "%s %s in %.3fs" name output
              duration;
            loop_chain (output :: acc_outputs) rest
        | None ->
            Logger.warnf ~tag:"invalid_tool_call" "%s %s" name
              (Yojson.Safe.to_string arguments);
            let%lwt () =
              Lwt_io.printlf "%sTool not found%s: %s" red color_reset name
            in
            let err = "[Tool error] " ^ name in
            loop_chain (err :: acc_outputs) rest)
  in
  loop_chain [] tool_calls
