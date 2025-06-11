open Message
open Agent
open Llm
open Tool
open Context
open Config
open Lwt.Infix

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

let read_input () =
  let%lwt () = Lwt_io.printf "%sYou%s: " blue color_reset in
  Lwt_io.read_line_opt Lwt_io.stdin >|= Option.map (fun s -> `Input s)

let handle_special_commands context input =
  match input with
  | "/tools" ->
      let%lwt () = Lwt_io.printf "%sAvailable tools%s:\n" magenta color_reset in
      let%lwt () =
        Tool.all_to_json () |> Yojson.Safe.pretty_to_string |> Lwt_io.printl
      in
      Lwt.return (`Continue context)
  | input when String.starts_with ~prefix:"/schema " input -> (
      let tool_name = String.sub input 8 (String.length input - 8) in
      match find_tool tool_name with
      | Some tool ->
          let%lwt () =
            tool.Tool.schema |> Yojson.Safe.pretty_to_string |> Lwt_io.printl
          in
          Lwt.return (`Continue context)
      | None ->
          let%lwt () =
            Lwt_io.printlf "%sTool not found%s: %s" red color_reset tool_name
          in
          Lwt.return (`Continue context))
  | _ -> Lwt.return (`Process input)

let repl_loop context =
  let shutdown_promise = Shutdown.wait_for_shutdown () in

  let rec loop context =
    let input_promise = read_input () in
    match%lwt
      Lwt.choose
        [
          (input_promise >|= fun x -> `Input x);
          (shutdown_promise >|= fun () -> `Shutdown);
        ]
    with
    | `Shutdown ->
        Logger.info "Shutdown signal received";
        Lwt_io.printl "\nGoodbye!"
    | `Input None ->
        Logger.info "EOF received";
        Lwt_io.printl "\nGoodbye!"
    | `Input (Some (`Input "exit")) ->
        Logger.info "Exit command received";
        Lwt_io.printl "\nGoodbye!"
    | `Input (Some (`Input input)) -> (
        match%lwt handle_special_commands context input with
        | `Continue new_context -> loop new_context
        | `Process input -> (
            Logger.info ~tag:"user_input" input;
            let context =
              add_turns context [ { role = `User; content = input } ]
            in
            let prompt = build_prompt context in
            let%lwt reply = fetch_reply prompt in
            match parse_tool_calls reply with
            | Some calls ->
                let%lwt outputs = run_tool_chain calls in
                let context =
                  add_turns context [ { role = `Assistant; content = reply } ]
                in
                let context =
                  add_turns context (List.map tool_response_message outputs)
                in
                let prompt = build_prompt context in
                let%lwt follow_up = fetch_reply prompt in
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset
                    follow_up
                in
                let context =
                  add_turns context
                    [
                      {
                        role = `Assistant;
                        content = "[Tool chain follow-up] " ^ follow_up;
                      };
                    ]
                in
                loop context
            | None ->
                Logger.info ~tag:"llm_reply" reply;
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset reply
                in
                let context =
                  add_turns context [ { role = `Assistant; content = reply } ]
                in
                loop context))
  in
  loop context
