open Llm_provider

let provider = lazy (Llm_factory.get_provider ())

let fetch_reply_messages messages =
  let provider = Lazy.force provider in
  provider.send_request messages
