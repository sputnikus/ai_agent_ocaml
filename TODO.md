# TODO

- Add automated testing for full conversation round trips
  * Test: User input → LLM response → Tool execution → Next LLM request
  * Verify format consistency across provider switches
  * Catch parsing/serialization mismatches early
- Refactor provider clients to return unified response types
  * Abstract away provider-specific response formats after parsing
  * Enable generic REPL handling regardless of provider
  * Simplify addition of new providers
- Implement provider-agnostic conversation state
  * Store conversations in provider-neutral format
  * Enable mid-conversation provider switching
  * Support conversation persistence and resumption
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
