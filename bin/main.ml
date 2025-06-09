open Ai_agent
open Lwt.Infix

let () =
  let handle_signal signal_name =
    Sys.(
      set_signal signal_name
        (Signal_handle
           (fun _ ->
             Shutdown.trigger_shutdown (Shutdown.signal_mapping signal_name))))
  in
  handle_signal Sys.sigint;
  handle_signal Sys.sigterm;
  Lwt_main.run
    ( Repl.print_intro () >>= fun () ->
      Repl.repl_loop (Context.init_context (Config.max_chars ())) )
