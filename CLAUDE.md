# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

- `dune build` - Build the project
- `AGENT_LOG='agent.log' AGENT_DEBUG=1 ./_build/default/bin/main.exe` - Run the AI agent executable
- `dune test` - Run tests
- `dune clean` - Clean build artifacts
- `dune fmt` - Format code
- `jj cdesc` - Automatically generate commit message

## Architecture Overview

This is an OCaml-based AI agent that provides a conversational interface with tool calling capabilities. The architecture consists of:

**Core Components:**
- `agent.ml` - Main agent logic and tool call parsing with system prompt management
- `llm.ml` - OpenAI API integration for chat completions
- `repl.ml` - Interactive REPL interface with colored output
- `tool.ml` - Tool system with `time` and `math` tools
- `message.ml` - Message formatting and display utilities
- `context.ml` - Conversation context management
- `config.ml` - Environment variable configuration
- `logger.ml` - Logging utilities
- `shutdown.ml` - Graceful shutdown handling

**Key Architecture Details:**
- Uses Lwt for asynchronous operations throughout
- Tools are defined with JSON schemas and async execution functions
- Agent parses LLM responses to detect tool calls vs natural language
- REPL provides colored terminal output with proper error handling
- Configuration via environment variables (OPENAI_API_KEY, AGENT_DEBUG, etc.)

**LLM Provider System:**
- Abstract interface in `llm_provider.ml` allows pluggable LLM backends
- `openai_provider.ml` - OpenAI ChatGPT API implementation
- `anthropic_provider.ml` - Anthropic Claude API implementation
- `llm_factory.ml` - Provider selection and instantiation
- `llm.ml` - Backwards-compatible wrapper maintaining existing interface

**Dependencies:**
- `lwt` + `lwt_ppx` for async programming
- `cohttp-lwt-unix` for HTTP requests to LLM APIs
- `yojson` for JSON parsing/generation
- `lwt_ssl` for HTTPS support

## Project Inspiration and Compliance

### Design Philosophy
This project follows the approach outlined in [How to Build an Agent](https://ampcode.com/how-to-build-an-agent), emphasizing:
- Minimal but powerful implementation (agent as conversation loop + tools + LLM)
- Trust in model's reasoning capabilities rather than complex control logic
- Tool-first design where external capabilities are exposed through simple interfaces
- Stateless conversation model with full history sent on each request

### 12-Factor Agents Compliance
Implementation aligns with [12-Factor Agents](https://github.com/humanlayer/12-factor-agents) principles:

**Implemented Factors:**
1. **Natural Language to Tool Calls** - Agent parses natural language and converts to structured tool calls via JSON
2. **Own Your Prompts** - System prompt with tool descriptions is fully controlled in `agent.ml:6-38`
3. **Tools are Structured Outputs** - All tools have consistent schema and return string results
4. **Own Your Control Flow** - Explicit tool call detection and execution in agent loop
5. **Compact Errors into Context Window** - Error handling integrated into conversation flow
6. **Small, Focused Agents** - Narrow focus on conversational agent with basic tool support
7. **Stateless Reducer** - Each interaction processes full conversation history without persistent state

**Areas for Future Development:**
- **Own Your Context Window** - Currently sends full history; implement context window management
- **Launch/Pause/Resume** - Add state persistence and resumption capabilities
- **Contact Humans with Tool Calls** - Implement human-in-the-loop tools
- **Unify Execution and Business State** - Integrate with external business systems
- **Trigger from Anywhere** - Currently CLI-only; add API/webhook endpoints

## Environment Variables

**LLM Provider Selection:**
- `LLM_PROVIDER` - Choose LLM provider: "openai" or "anthropic" (default: openai)

**Required (based on provider):**
- `OPENAI_API_KEY` - OpenAI API key (required when LLM_PROVIDER=openai)
- `ANTHROPIC_API_KEY` - Anthropic API key (required when LLM_PROVIDER=anthropic)

**Optional:**
- `OPENAI_MODEL` - OpenAI model to use (default: gpt-3.5-turbo-0125)
- `ANTHROPIC_MODEL` - Anthropic model to use (default: claude-3-haiku-20240307)
- `AGENT_NAME` - Agent name (default: CamelGent)
- `AGENT_DEBUG` - Set to "1" to enable debug logging
- `AGENT_TOOL_TIMEOUT` - Tool execution timeout in seconds (default: 10)
- `AGENT_LOG` - Log file path
- `AGENT_MAX_CHARS` - Max characters for responses (default: 8000)

## Development Guidelines

### Adding New Tools
1. Define tool in `tool.ml` with name, description, schema, and run function
2. Add to `tools` list in `tool.ml`
3. Tools should be simple, focused, and return descriptive string results
4. Follow the pattern: `Yojson.Safe.t -> string Lwt.t`

### Extending Agent Capabilities
- Keep core agent logic minimal and trust LLM reasoning
- Prefer adding tools over complex control flow
- Maintain stateless design for predictability
- Consider 12-factor principles when adding features
- Always format created code using Dune formatter
