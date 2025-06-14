open Agent
open Message

type t = { system : message; turns : message list; max_chars : int }

let trim_history ~max_chars turns =
  let rec loop acc remaining =
    match remaining with
    | [] -> acc
    | msg :: rest ->
        let total =
          List.fold_left (fun sum m -> sum + String.length m.content) 0 acc
        in
        if total + String.length msg.content > max_chars then acc
        else loop (msg :: acc) rest
  in
  (* We are trimming the oldest, keeping the most recent*)
  loop [] (List.rev turns)

let build_messages context =
  let trimmed = trim_history ~max_chars:context.max_chars context.turns in
  context.system :: trimmed

(* Deprecated: kept for backwards compatibility *)
let build_prompt context =
  let messages = build_messages context in
  `Assoc
    [
      ("model", `String (Config.openai_model ()));
      ( "messages",
        `List (List.map yojson_of_message messages) );
    ]

let init_context max_chars =
  let system = { role = `System; content = system_prompt_with_tools } in
  { system; turns = []; max_chars }

let add_turns context messages =
  { context with turns = context.turns @ messages }
