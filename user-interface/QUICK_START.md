# Quick Start Guide

## Development Setup

### 1. Install Frontend Dependencies

```bash
cd user-interface
npm install
```

### 2. Install Backend Server Dependencies

```bash
cd user-interface/server
npm install
```

### 3. Configure Environment

Copy the example environment file:

```bash
cd user-interface
cp .env.example .env
```

Edit `.env` if you need to change the API URL (default: `http://localhost:3001/api`).

### 4. Start Backend Server

Terminal 1:

```bash
cd user-interface/server
npm run server
```

You should see:
```
Computational Scheme Theory API server running on http://localhost:3001
```

### 5. Start Frontend Development Server

Terminal 2:

```bash
cd user-interface
npm run dev
```

You should see:
```
  VITE v7.x.x  ready in xxx ms

  ➜  Local:   http://localhost:5173/
  ➜  Network: use --host to expose
```

### 6. Open in Browser

Navigate to `http://localhost:5173` to see the UI.

## Usage

### Natural Language Queries

1. Click the "Natural Language Query" tab
2. Type a query like: `compute H1 for program test`
3. Click "Execute Query"
4. View results in the right panel

### Scheme Code Editor

1. Click the "Scheme Code Editor" tab
2. Write or paste R5RS Scheme code
3. Click "Compute H¹" or "Compute V(G)"
4. View results in the right panel

## Troubleshooting

### Backend Won't Start

- Ensure Racket is installed: `racket --version`
- Check that `racket-mcp/src/mcp-server.rkt` exists
- Verify Node.js version: `node --version` (should be 18+)

### Frontend Can't Connect to Backend

- Ensure backend is running on port 3001
- Check browser console for errors
- Verify `.env` file has correct `VITE_API_URL`

### CORS Errors

- Backend server includes CORS middleware
- If issues persist, check that backend is running before frontend

## Production Build

```bash
npm run build
npm run preview
```

This builds the frontend and serves it from the `dist` folder.
