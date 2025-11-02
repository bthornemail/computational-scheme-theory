# Server Troubleshooting Guide

## Common Issues

### Server Won't Start

1. **Check Node.js version**
   ```bash
   node --version  # Should be 18+
   ```

2. **Install dependencies**
   ```bash
   cd server
   npm install
   ```

3. **Check Racket installation**
   ```bash
   racket --version  # Should be 8.0+
   ```

4. **Verify paths**
   ```bash
   # Check if racket-unified directory exists
   ls -la ../../racket-unified/src/algorithms/
   ```

### Server Starts But API Calls Fail

1. **Check server logs**
   - Look for error messages in the console
   - Check for Racket-related errors

2. **Test health endpoint**
   ```bash
   curl http://localhost:3001/health
   ```

3. **Check Racket script execution**
   ```bash
   # Try running a simple Racket script
   racket -e "(displayln \"Hello\")"
   ```

### "Racket process exited with code X"

- **Code 1**: Usually means Racket syntax error or module not found
- **Code 127**: Racket not found in PATH
- **Code 2**: File not found or path issue

**Solutions:**
- Verify Racket is installed: `which racket`
- Check that `racket-unified/src` directory exists
- Ensure all Racket modules are compiled

### "Failed to parse Racket output"

- Racket script may be outputting non-JSON
- Check server logs for actual Racket output
- Verify Racket modules are loaded correctly

### CORS Errors

- Server already includes CORS middleware
- If issues persist, check browser console
- Verify backend is running before frontend

## Testing the Server

### Manual Test

```bash
# Start server
cd server
npm run server

# In another terminal, test health endpoint
curl http://localhost:3001/health

# Test compute H1
curl -X POST http://localhost:3001/api/compute-h1 \
  -H "Content-Type: application/json" \
  -d '{"source_code": "(lambda (x) x)"}'
```

### Debug Mode

Add more logging to see what's happening:

```javascript
// In index.js, add before executeRacketScript calls:
console.log('Executing Racket script:', script.substring(0, 100));
```

## Alternative: Mock Mode

If Racket is not available, you can modify the server to return mock data for testing:

```javascript
const MOCK_MODE = process.env.MOCK_MODE === 'true';

async function computeH1(sourceCode) {
  if (MOCK_MODE) {
    return {
      success: true,
      h1: 0,
      bindings: 1,
      simplices_0: 1,
      simplices_1: 0,
      simplices_2: 0,
    };
  }
  // ... rest of implementation
}
```

Then run with:
```bash
MOCK_MODE=true npm run server
```
