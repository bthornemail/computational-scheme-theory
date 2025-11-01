# Computational Scheme Theory MCP Server

Model Context Protocol (MCP) server extension for the Computational Scheme Theory unified Lisp substrate. This server exposes computational capabilities to AI assistants like Claude Desktop through the standard MCP protocol.

## Overview

The MCP server provides a bridge between AI assistants and the Computational Scheme Theory system, enabling:

- **H¹ Cohomology Computation** - Analyze Scheme programs and compute topological invariants
- **Natural Language Processing** - Convert natural language queries to executable M-expressions
- **Hypothesis Validation** - Test mathematical relationships between complexity metrics
- **Knowledge Graph Access** - Query semantic structures from program analysis

## Installation

### Prerequisites

- **Racket** 7.0 or later
- **racket-unified** package (parent directory)

### Setup

1. Ensure `racket-unified` is accessible (same parent directory)
2. The MCP server automatically finds and uses the core package

## Configuration

### Claude Desktop

Add to your Claude Desktop configuration file (usually `~/Library/Application Support/Claude/claude_desktop_config.json` on macOS, or similar location on other platforms):

```json
{
  "mcpServers": {
    "computational-scheme": {
      "command": "racket",
      "args": ["/absolute/path/to/racket-mcp/src/mcp-server.rkt"]
    }
  }
}
```

**Important**: Use an absolute path to `mcp-server.rkt`. The path should point to the actual file location.

### Other MCP Clients

For other MCP-compatible clients, configure them to:
- Command: `racket`
- Arguments: `[path/to/racket-mcp/src/mcp-server.rkt]`
- Transport: stdio (default)

## Available Tools

### compute_h1

Compute H¹ cohomology from Scheme source code.

**Input:**
```json
{
  "source_code": "(lambda (x) x)"
}
```

**Output:**
```json
{
  "success": true,
  "h1": 0,
  "bindings": 1,
  "simplices": {
    "0": 1,
    "1": 0,
    "2": 0
  },
  "beta0": 1,
  "beta1": 0
}
```

### compute_vg

Compute V(G) cyclomatic complexity (requires Racket V(G) service).

**Input:**
```json
{
  "source_code": "(lambda (x) x)"
}
```

**Output:**
```json
{
  "success": true,
  "v_g": 1
}
```

### validate_hypothesis

Validate the hypothesis H¹ = V(G) - k.

**Input:**
```json
{
  "h1": 0,
  "v_g": 1,
  "k": 0,
  "tolerance": 0
}
```

**Output:**
```json
{
  "valid": false,
  "difference": 1,
  "message": "Hypothesis violated: H¹=0 ≠ V(G)-k=1 (diff: 1)"
}
```

### process_natural_language

Convert natural language query to M-expression using SGP-ASLN.

**Input:**
```json
{
  "query": "compute H1 for program test"
}
```

**Output:**
```json
{
  "success": true,
  "m_expression": {
    "op": "computeH1",
    "args": ["test"]
  },
  "raw_query": "compute H1 for program test"
}
```

## Resources

The server provides access to data resources:

- `computational-scheme://bindings` - Binding algebra data
- `computational-scheme://topology` - Scope topology data  
- `computational-scheme://knowledge-graph` - Semantic knowledge graph

*Note: Resource reading is currently in development and requires program context.*

## Usage Examples

### Example 1: Compute H¹

User (to AI assistant):
> "Compute H¹ for the program `(lambda (x) (lambda (y) (+ x y)))`"

AI Assistant calls MCP:
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "compute_h1",
    "arguments": {
      "source_code": "(lambda (x) (lambda (y) (+ x y)))"
    }
  },
  "id": 1
}
```

Response:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "success": true,
    "h1": 0,
    "bindings": 2,
    ...
  }
}
```

### Example 2: Natural Language Processing

User:
> "Convert this to M-expression: compute H1"

AI Assistant calls:
```json
{
  "method": "tools/call",
  "params": {
    "name": "process_natural_language",
    "arguments": {
      "query": "compute H1"
    }
  }
}
```

## Architecture

```
AI Assistant (Claude Desktop, etc.)
         │
         │ JSON-RPC 2.0 (stdio)
         ▼
   racket-mcp/
   └── src/
       ├── mcp-server.rkt      ← Main server loop
       ├── json-rpc.rkt         ← Protocol handler
       ├── mcp-tools.rkt        ← Tool wrappers
       └── mcp-resources.rkt    ← Resource definitions
         │
         │ (require)
         ▼
   racket-unified/
   └── src/
       ├── api.rkt              ← Core API
       └── nlp/                  ← NLP capabilities
```

## Non-Destructive Extension

This MCP server is a **non-destructive extension**:
- ✅ Uses existing `racket-unified` public API
- ✅ No modifications to core package
- ✅ Completely additive
- ✅ Can be removed without affecting core

## Troubleshooting

### Server Not Starting

- Check that Racket is in your PATH
- Verify absolute path to `mcp-server.rkt` is correct
- Check file permissions (must be executable/readable)

### Tools Not Working

- Ensure `racket-unified` is in parent directory
- Check that core API functions are accessible
- Review stderr output for error messages

### Parse Errors

- Verify JSON-RPC request format
- Check tool input schema matches your parameters
- Review error messages in response

## Development

### Testing

To test the server manually:

```bash
# Start server
racket src/mcp-server.rkt

# Send test request (paste into terminal)
{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}

# Server responds with initialization
```

### Adding New Tools

1. Add tool handler function in `src/mcp-tools.rkt`
2. Add tool definition to `mcp-tool-definitions` list
3. Tool automatically available via `tools/list` and `tools/call`

## License

Part of the Computational Scheme Theory project.

---

**Version**: 0.1.0  
**Protocol**: MCP 2024-11-05  
**Status**: Production Ready

