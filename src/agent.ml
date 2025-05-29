open Message

let initial_history = [
  { role = `System; content = "You are a helpful assistant." }
]

let add_turn history user_input assistant_reply =
  history @ [
    { role = `User; content = user_input };
    { role = `Assistant; content = assistant_reply }
  ]
