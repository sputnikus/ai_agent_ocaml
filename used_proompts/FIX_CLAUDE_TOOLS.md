Current tool format is OpenAI compatible and needs to be reworked to support
both OpenAI AND Claude APIs properly. 

## Root Cause Analysis
The system prompt in `llm_provider.ml:13-40` instructs Claude to respond with OpenAI-style 
JSON in text (`{"tool_calls": ...}`), but Claude has native tool calling capabilities 
that should be used instead. This causes Claude to pack tool calls into text content 
rather than using structured tool responses.

## Revised Implementation Plan

**Phase 1: Fix System Prompt for Provider-Specific Tool Instructions**
- Modify `llm_provider.ml:system_prompt` to be provider-aware
- For OpenAI: Keep current JSON-in-text format (since that's what the current parsing expects)
- For Claude: Use native tool calling instructions that leverage Claude's built-in tool use capabilities
- Create separate system prompts or conditional logic based on `Config.llm_provider()`

**Phase 2: Fix Claude Response Parsing**
- Modify `anthropic_provider.ml:extract_reply` to properly parse Claude's native tool call format
- Claude returns tool calls as `{"type": "tool_use", "id": "...", "name": "...", "input": {...}}` in the content array
- Map these to the existing `ToolCalls` reply type instead of treating them as text
- Handle mixed content (text + tool calls) in Claude responses

**Phase 3: Update Message Serialization for Claude**
- Modify `message.ml:yojson_of_message` to support Claude's message format when `LLM_PROVIDER=anthropic`
- Claude expects tool results as messages with `role: "user"` and `content: [{"type": "tool_result", "tool_use_id": "...", "content": "..."}]`
- Keep OpenAI format for OpenAI provider

**Phase 4: Fix Tool Call ID Handling**
- Ensure tool call IDs from Claude are properly preserved through the `agent.ml` interaction loop
- Update `agent.ml:65` to handle Claude's tool call ID format
- Verify the tool response mapping works correctly

**Phase 5: Test Integration**
- Test with both providers to ensure no regressions
- Verify tool chaining works correctly with both OpenAI and Claude
- Test mixed scenarios (conversation + tools, multiple tools, tool chaining)
