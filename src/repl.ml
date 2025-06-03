open Message
open Agent
open Llm
open Tool

(* REPL colors *)
let color_reset = "\027[0m"
let blue = "\027[34m"
let green = "\027[32m"
let yellow = "\027[33m"
let magenta = "\027[35m"
let red = "\027[31m"

let print_intro () =
  let legend =
    [
      (green, "Assistant");
      (blue, "You");
      (yellow, "Tool Output");
      (red, "System messages");
    ]
  in
  let%lwt () = Lwt_io.printl "Welcome to OCaml Agent 🧠" in
  let%lwt () =
    Lwt_io.printl "Type your message, or type 'exit' (Ctrl+D) to quit.\n"
  in
  let%lwt () =
    legend
    |> List.map (fun (c, label) ->
           Lwt_io.printlf "%s%s%s = %s" c label color_reset
             (String.lowercase_ascii label))
    |> Lwt.join
  in
  Lwt_io.printl ""

let rec repl_loop history =
  let%lwt () = Lwt_io.print (blue ^ "You" ^ color_reset ^ ": ") in
  let%lwt user_input = Lwt_io.read_line_opt Lwt_io.stdin in
  match user_input with
  | None -> Lwt_io.printl "\nGoodbye!"
  | Some input -> (
      if input = "exit" then Lwt_io.printl "Exiting."
      else
        let prompt =
          build_prompt (history @ [ { role = `User; content = input } ])
        in
        let%lwt reply = fetch_reply prompt in
        match try_parse_tool_call reply with
        | Some (tool_name, tool_args_json) -> (
            match find_tool tool_name with
            | Some tool ->
                let args = Yojson.Safe.to_string tool_args_json in
                let%lwt () =
                  Lwt_io.printf "%sTool%s: %s(%s)\n\n" yellow color_reset
                    tool_name args
                in
                let%lwt tool_output = tool.run tool_args_json in
                let history = add_turn history input reply in
                let history = history @ [ tool_response_message tool_output ] in
                let prompt = build_prompt history in
                let%lwt follow_up = fetch_reply prompt in
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset
                    follow_up
                in
                let history =
                  add_turn history
                    ("[Follow-up after tool] " ^ tool_output)
                    follow_up
                in
                repl_loop history
            | None ->
                let%lwt () =
                  Lwt_io.printlf "%sUnknown tool requested%s: %s" red
                    color_reset tool_name
                in
                let history = add_turn history input reply in
                let history =
                  add_turn history input ("[Tool error] " ^ tool_name)
                in
                repl_loop history)
        | None ->
            let%lwt () =
              Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset reply
            in
            let history = add_turn history input reply in
            repl_loop history)
