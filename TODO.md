# TODO

- Separate system message from the context and implement proper tokenization
  * Anthropic expects system message in separate parameter and for OpenAI we can
  easily prepend in to the payload
- Finish the minimal implementation from https://ampcode.com/how-to-build-an-agent
- Add TUI parameters and integrate them with environment variables
- Delve deeper into agentic workflows
  * Enhance tool chaining with follow-up planning
  * Introduce stateful working memory
  * Improve structured tool outputs
- TUI eyecandy with https://github.com/leostera/minttea
