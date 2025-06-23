type role = [ `System | `User | `Assistant | `Tool ]
type tool_function = { name : string; arguments : Yojson.Safe.t }

type tool_call = {
  id : string option; (* OpenAI provides this *)
  function_ : tool_function;
}

type tool_response = {
  tool_call_id : string option; (* OpenAI provides this, same as tool_call.id *)
  output_ : string;
}

type content =
  | Text of string
  | ToolCalls of tool_call list
  | ToolResponse of tool_response (* For tool outputs *)

type message = { role : role; content : content }

let yojson_of_role = function
  | `System -> `String "system"
  | `User -> `String "user"
  | `Assistant -> `String "assistant"
  | `Tool -> `String "tool"

let yojson_of_tool_function { name; arguments } =
  `Assoc
    [
      ("name", `String name);
      (* Arguments always as string *)
      ("arguments", `String (Yojson.Safe.to_string arguments));
    ]

let yojson_of_tool_call { id; function_ } =
  match String.lowercase_ascii (Config.llm_provider ()) with
  | "anthropic" ->
      `Assoc
        [
          ("id", match id with Some i -> `String i | None -> `Null);
          ("type", `String "tool_use");
          ("name", `String function_.name);
          ("input", function_.arguments);
        ]
  | "openai" | _ ->
      `Assoc
        [
          ("id", match id with Some i -> `String i | None -> `Null);
          ("type", `String "function");
          ("function", yojson_of_tool_function function_);
        ]

let yojson_of_message { role; content } =
  match String.lowercase_ascii (Config.llm_provider ()) with
  | "anthropic" -> (
      match content with
      | Text text ->
          `Assoc [ ("role", yojson_of_role role); ("content", `String text) ]
      | ToolCalls _calls ->
          (* Claude doesn't use tool_calls in messages - this shouldn't happen *)
          `Assoc
            [
              ("role", yojson_of_role role);
              ("content", `List (List.map yojson_of_tool_call _calls));
            ]
      | ToolResponse { tool_call_id; output_ } ->
          (* Claude expects tool results as user messages with structured content *)
          `Assoc
            [
              ("role", `String "user");
              ( "content",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "tool_result");
                        ( "tool_use_id",
                          match tool_call_id with
                          | Some i -> `String i
                          | None -> `Null );
                        ("content", `String output_);
                      ];
                  ] );
            ])
  | "openai" | _ -> (
      match content with
      | Text text ->
          `Assoc [ ("role", yojson_of_role role); ("content", `String text) ]
      | ToolCalls calls ->
          `Assoc
            [
              ("role", yojson_of_role role);
              ("content", `Null);
              ("tool_calls", `List (List.map yojson_of_tool_call calls));
            ]
      | ToolResponse { tool_call_id; output_ } ->
          `Assoc
            [
              ("role", yojson_of_role role);
              ("content", `String output_);
              ( "tool_call_id",
                match tool_call_id with Some i -> `String i | None -> `Null );
            ])

(* Helper constructors *)
let system text = { role = `System; content = Text text }
let user text = { role = `User; content = Text text }
let assistant text = { role = `Assistant; content = Text text }
let tool_calls calls = { role = `Assistant; content = ToolCalls calls }
let tool_response response = { role = `Tool; content = ToolResponse response }

(* Helper getters *)
let content_as_str m =
  match m.content with
  | Text text -> text
  | ToolCalls calls ->
      Yojson.Safe.to_string (`List (List.map yojson_of_tool_call calls))
  | ToolResponse { tool_call_id; output_ } ->
      Yojson.Safe.to_string
        (`Assoc
           [
             ( "tool_call_id",
               match tool_call_id with Some i -> `String i | None -> `Null );
             ("content", `String output_);
           ])
