#!/bin/bash
# Script to restart the backend server

echo "Stopping any existing server on port 3001..."
lsof -ti:3001 | xargs kill -9 2>/dev/null || true

sleep 1

echo "Starting backend server..."
cd /home/main/computational-scheme-theory/user-interface/server
node index.js &

sleep 2

echo "Testing server..."
curl -s http://localhost:3001/health | jq . || echo "Server might still be starting..."

echo ""
echo "Backend server should now be running on http://localhost:3001"
echo "Check the output above for any errors."
