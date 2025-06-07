open Agent
open Message

type t = {
  system : message;
  turns : message list;
  max_chars : int;
}

let trim_history ~max_chars turns =
  let rec loop acc remaining =
    match remaining with
    | [] -> List.rev acc
    | msg :: rest ->
        let total = List.fold_left (fun sum m -> sum + String.length m.content) 0 acc in
        if total + String.length msg.content > max_chars then
          List.rev acc
        else
          loop (msg :: acc) rest
  in
  (* We are trimming the oldest, keeping the most recent*)
  loop [] (List.rev turns)

let build_prompt context =
  let trimmed = trim_history ~max_chars:context.max_chars context.turns in
  `Assoc
    [
      ("model", `String (Config.model ()));
      ("messages", `List (List.map yojson_of_message (context.system :: trimmed)));
    ]

let init_context max_chars =
  let system = { role = `System; content = system_prompt_with_tools } in
  { system; turns = []; max_chars }

let add_turns context messages =
  { context with turns = context.turns @ messages }
