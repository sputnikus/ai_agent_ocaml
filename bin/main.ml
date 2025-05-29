open Ai_agent

let () =
  Lwt_main.run (Repl.repl_loop Agent.initial_history)
