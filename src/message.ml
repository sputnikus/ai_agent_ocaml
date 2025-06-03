type role = [ `System | `User | `Assistant ]
type message = { role : role; content : string }

let yojson_of_role = function
  | `System -> `String "system"
  | `User -> `String "user"
  | `Assistant -> `String "assistant"

let yojson_of_message { role; content } =
  `Assoc [ ("role", yojson_of_role role); ("content", `String content) ]
