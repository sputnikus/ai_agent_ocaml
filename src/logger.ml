let log_channel =
  lazy
    (match Config.log_file () with
    | Some path ->
        let flags =
          if Config.debug_enabled () then
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

let info ?(tag = "info") msg = log "INFO" tag msg
let warn ?(tag = "warn") msg = log "WARN" tag msg
let error ?(tag = "error") msg = log "ERROR" tag msg
let result ?(tag = "result") msg = log "RESULT" tag msg

let debug ?(tag = "debug") msg =
  if Config.debug_enabled () then log "DEBUG" tag msg
