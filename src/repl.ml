open Message
open Agent
open Llm
open Llm_provider
open Tool
open Context
open Config
open Lwt.Infix

let print_intro () =
  let legend =
    [
      (green, "Assistant", "Model text reply");
      (blue, "You", "Your input");
      (yellow, "Tool Output", "Data returned by local tool");
      (red, "System messages", "Human readable errors");
    ]
  in
  let%lwt () =
    Lwt_io.printl
      {|
               ,,__      ██████╗ ███████╗███╗   ██╗████████╗
     ..  ..   / o._)    ██╔════╝ ██╔════╝████╗  ██║╚══██╔══╝
    /--'/--\  \-'||     ██║  ███╗█████╗  ██╔██╗ ██║   ██║
   /        \_/ / |     ██║   ██║██╔══╝  ██║╚██╗██║   ██║
 .'\  \__\  __.'.'      ╚██████╔╝███████╗██║ ╚████║   ██║
   )\ |  )\ |            ╚═════╝ ╚══════╝╚═╝  ╚═══╝   ╚═╝
  // \ // \
 ||_  \|_  \_           Type your message, or type '/exit' (Ctrl+D) to quit.
 '--' '--'' '--'
  |}
  in
  let%lwt () =
    legend
    |> List.map (fun (c, label, desc) ->
           Lwt_io.printlf "%s%s%s = %s" c label color_reset desc)
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

let handle_tool_calls context calls =
  let parsed_calls = parse_tool_calls calls in
  match parsed_calls with
  | Some parsed ->
      let%lwt new_context = run_interactive_tools context parsed in
      Lwt.return new_context
  | None ->
      let%lwt () =
        Lwt_io.printf "%sAssistant%s: Invalid tool chain.\n\n" yellow
          color_reset
      in
      Lwt.return (add_turns context [ assistant "Malformed response" ])

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
    | `Input (Some (`Input "/exit")) ->
        Logger.info "Exit command received";
        Lwt_io.printl "\nGoodbye!"
    | `Input (Some (`Input input)) -> (
        match%lwt handle_special_commands context input with
        | `Continue new_context -> loop new_context
        | `Process input -> (
            Logger.info ~tag:"user_input" input;
            let context = add_turns context [ user input ] in
            let messages = build_messages context in
            let%lwt reply = fetch_reply_messages messages in
            match reply with
            | ToolCalls calls ->
                let%lwt context = handle_tool_calls context calls in
                loop context
            | MixedContent (text, calls) ->
                Logger.info ~tag:"llm_reply" text;
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset text
                in
                let context = add_turns context [ assistant text ] in
                let%lwt context = handle_tool_calls context calls in
                loop context
            | Content text ->
                Logger.info ~tag:"llm_reply" text;
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: %s\n\n" green color_reset text
                in
                let context = add_turns context [ assistant text ] in
                loop context
            | Malformed exc_string ->
                let%lwt () =
                  Lwt_io.printf "%sAssistant%s: Malformed response: %s\n\n" red
                    color_reset exc_string
                in
                let context =
                  add_turns context [ assistant "Malformed response" ]
                in
                loop context))
  in
  loop context
