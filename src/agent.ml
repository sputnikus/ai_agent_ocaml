open Tool
open Config
open Context
open Message

type tool_call = {
  id : string option;
  name : string;
  arguments : Yojson.Safe.t;
}

let parse_tool_calls calls =
  try
    let open Yojson.Safe.Util in
    let parsed =
      List.filter_map
        (fun call ->
          try
            let id = call |> member "id" |> to_string_option in
            let func = call |> member "function" in
            let name = func |> member "name" |> to_string in
            let args_str = func |> member "arguments" |> to_string in
            let args = Yojson.Safe.from_string args_str in
            Some { id; name; arguments = args }
          with _ -> None)
        calls
    in
    Some parsed
  with _ -> None

(* New function for interactive tool chaining *)
let run_interactive_tools context initial_calls =
  let rec interaction_loop context = function
    | [] ->
        (* No more tool calls - return final context *)
        Lwt.return context
    | { id; name; arguments } :: rest -> (
        (* Handle single tool *)
        match find_tool name with
        | Some tool -> (
            let args = Yojson.Safe.to_string arguments in
            Logger.infof ~tag:"llm_tool" "%s(%s)" name args;
            let%lwt () =
              Lwt_io.printf "%sTool%s: %s(%s)\n\n" yellow color_reset name args
            in

            (* Run the tool *)
            let start_time = Unix.gettimeofday () in
            let%lwt output =
              try%lwt tool.run arguments
              with exn ->
                let msg = Printexc.to_string exn in
                Logger.errorf ~tag:"tool_call" "[tool fail] %s %s: %s" name args
                  msg;
                Lwt.return ("[Tool failed] " ^ name ^ ": " ^ msg)
            in
            let duration = Unix.gettimeofday () -. start_time in
            Logger.resultf ~tag:"tool_call" "%s %s in %.3fs" name output
              duration;

            (* Add tool call and result to context *)
            let context =
              add_turns context
                [
                  tool_calls [ { id; function_ = { name; arguments } } ];
                  tool_response { tool_call_id = id; output_ = output };
                ]
            in

            (* Get LLM's next decision *)
            let messages = build_messages context in
            Logger.infof ~tag:"messages_to_llm" "Sending messages: %s"
              (Yojson.Safe.to_string
                 (`List (List.map yojson_of_message messages)));
            let%lwt reply = Llm.fetch_reply_messages messages in

            match reply with
            | ToolCalls next_calls -> (
                (* LLM wants more tools - process them *)
                let%lwt parsed = Lwt.return (parse_tool_calls next_calls) in
                match parsed with
                | Some tools -> interaction_loop context tools
                | None -> Lwt.return context)
            | Content text ->
                (* LLM is done with tools - add final response *)
                Logger.info ~tag:"llm_reply" text;
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset text
                in
                let context = add_turns context [ assistant text ] in
                Lwt.return context
            | Malformed err ->
                Logger.error ~tag:"llm_reply" err;
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: Malformed response: %s\n\n" red
                    color_reset err
                in
                Lwt.return context)
        | None ->
            (* Skip invalid tool and continue with rest *)
            interaction_loop context rest)
  in
  interaction_loop context initial_calls
