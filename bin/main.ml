open Ai_agent
open Lwt.Infix

let () =
  Lwt_main.run
    (Repl.print_intro () >>= fun () -> Repl.repl_loop Agent.initial_history)
