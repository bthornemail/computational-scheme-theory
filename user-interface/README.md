# Computational Scheme Theory - User Interface

A modern React TypeScript web interface for Computational Scheme Theory computations.

## Features

- **Natural Language Query Interface**: Ask questions in plain English like "compute H1 for program test"
- **Scheme Code Editor**: Write and execute R5RS Scheme code directly
- **Real-time Results**: View H? cohomology, V(G) cyclomatic complexity, and hypothesis validation results
- **Modern UI**: Clean, responsive design with dark/light mode support

## Prerequisites

- **Node.js** 18+ and npm
- **Racket** 8.0+ (for backend computations)
- The `racket-unified` and `racket-mcp` packages from the parent directory

## Setup

### 1. Install Dependencies

```bash
cd user-interface
npm install
```

### 2. Install Backend Server Dependencies

```bash
cd server
npm install
```

### 3. Start the Backend API Server

In one terminal:

```bash
cd user-interface/server
npm run server
```

The API server will start on `http://localhost:3001`.

### 4. Start the Frontend Development Server

In another terminal:

```bash
cd user-interface
npm run dev
```

The UI will be available at `http://localhost:5173` (or the port Vite assigns).

## Environment Variables

Create a `.env` file in the `user-interface` directory:

```env
VITE_API_URL=http://localhost:3001/api
```

## Usage

### Natural Language Queries

Use the "Natural Language Query" tab to ask questions like:
- `compute H1 for program test`
- `compute V(G) for program test`
- `validate hypothesis for program test`
- `export polynomial for program test`
- `get pattern dimensions for program test`

### Scheme Code Editor

Use the "Scheme Code Editor" tab to:
1. Write or paste R5RS Scheme code
2. Click "Compute H?" or "Compute V(G)" to analyze the code
3. View results in the results panel

### Results Display

The results panel shows:
- **H? Cohomology**: Topological complexity metric
- **V(G) Cyclomatic Complexity**: Traditional complexity metric
- **Hypothesis Validation**: Tests whether H? = V(G) - k

## Architecture

```
???????????????????
?  React UI       ?
?  (Port 5173)    ?
???????????????????
         ? HTTP/REST
         ?
???????????????????
?  Express API    ?
?  (Port 3001)    ?
???????????????????
         ? stdio/JSON-RPC
         ?
???????????????????
?  Racket MCP     ?
?  Server        ?
???????????????????
         ?
         ?
???????????????????
?  Racket Unified ?
?  Pipeline       ?
???????????????????
```

## Development

### Frontend Development

```bash
npm run dev      # Start dev server with hot reload
npm run build    # Build for production
npm run preview  # Preview production build
npm run lint     # Run ESLint
```

### Backend Development

```bash
cd server
npm run dev      # Start with watch mode
npm run server   # Start normally
```

## Troubleshooting

### Backend Server Won't Start

1. Ensure Racket is installed: `racket --version`
2. Verify the MCP server path is correct
3. Check that `racket-unified` is accessible from the parent directory

### API Calls Fail

1. Ensure the backend server is running on port 3001
2. Check browser console for CORS errors
3. Verify `VITE_API_URL` environment variable is set correctly

### Racket MCP Communication Errors

The MCP server uses stdio communication. If you see errors:
1. Verify Racket is in your PATH
2. Check file permissions on `racket-mcp/src/mcp-server.rkt`
3. Review server logs for detailed error messages

## Project Structure

```
user-interface/
??? src/
?   ??? components/          # React components
?   ?   ??? NaturalLanguageQuery.tsx
?   ?   ??? SchemeEditor.tsx
?   ?   ??? ResultsDisplay.tsx
?   ??? services/           # API service layer
?   ?   ??? api.ts
?   ??? App.tsx             # Main app component
?   ??? main.tsx            # Entry point
??? server/                 # Backend API server
?   ??? index.js            # Express server
?   ??? package.json
??? package.json
??? vite.config.ts
```

## License

Part of the Computational Scheme Theory project.
