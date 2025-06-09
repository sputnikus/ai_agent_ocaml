let log_channel =
  lazy
    (match Config.log_file () with
    | Some path ->
        let flags =
          if Config.debug () then
            [ Open_creat; Open_trunc; Open_text; Open_wronly ]
          else [ Open_creat; Open_append; Open_text; Open_wronly ]
        in
        open_out_gen flags 0o644 path
    | None -> stderr)

let timestamp () =
  let open Unix in
  let tm = localtime (time ()) in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

let log level tag message =
  let out = Lazy.force log_channel in
  Printf.fprintf out "[%s] [%s] [%s] %s\n%!" (timestamp ()) level tag message

(* Raw text loggers *)
let info ?(tag = "info") msg = log "INFO" tag msg
let warn ?(tag = "warn") msg = log "WARN" tag msg
let error ?(tag = "error") msg = log "ERROR" tag msg
let result ?(tag = "result") msg = log "RESULT" tag msg
let debug ?(tag = "debug") msg = if Config.debug () then log "DEBUG" tag msg

(* Formatting helper functions *)
let infof ~tag fmt = Printf.ksprintf (info ~tag) fmt
let warnf ~tag fmt = Printf.ksprintf (warn ~tag) fmt
let errorf ~tag fmt = Printf.ksprintf (error ~tag) fmt
let debugf ~tag fmt = Printf.ksprintf (debug ~tag) fmt
let resultf ~tag fmt = Printf.ksprintf (result ~tag) fmt
