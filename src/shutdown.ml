let signal_mapping = function
  | x when x = Sys.sigint -> "SIGINT"
  | x when x = Sys.sigterm -> "SIGTERM"
  | _ -> "UNKNOWN"

let shutdown_signal = Lwt_condition.create ()

let wait_for_shutdown () =
  Lwt_condition.wait shutdown_signal

let trigger_shutdown signal =
  Logger.infof ~tag:"shutdown" "Received signal: %s" signal;
  Lwt_condition.broadcast shutdown_signal ();
  ()
