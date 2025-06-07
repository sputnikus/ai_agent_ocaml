open Ai_agent
open Lwt.Infix

let () =
  Lwt_main.run
    ( Repl.print_intro () >>= fun () ->
      Repl.repl_loop (Context.init_context (Config.max_chars ())) )
