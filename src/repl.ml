open Message
open Agent
open Llm

let rec repl_loop history =
  let%lwt () = Lwt_io.print "> " in
  let%lwt user_input = Lwt_io.read_line_opt Lwt_io.stdin in
  match user_input with
  | None -> Lwt_io.printl "\nGoodbye!"
  | Some input ->
    if input = "exit" then
      Lwt_io.printl "Exiting."
    else
      let prompt = build_prompt (history @ [{ role = `User; content = input }]) in
      let%lwt reply = fetch_reply prompt in
      let%lwt () = Lwt_io.printf "Assistant: %s\n\n" reply in
      let history = add_turn history input reply in
      repl_loop history
