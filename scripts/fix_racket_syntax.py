#!/usr/bin/env python3
"""Fix Racket cfg-builder.rkt syntax"""
import re

with open('racket-metrics/cfg-builder.rkt', 'r') as f:
    content = f.read()

# The issue is likely in the build-cfg function closing
# Let's ensure proper structure
lines = content.split('\n')

# Find the build-cfg function and fix closing
fixed_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    fixed_lines.append(line)
    
    # Check if this is the problematic closing line
    if i == 44 and '(hash))))))' in line:
        # This should close: cfg, let all-nodes, let base-nodes, let-values, let*, define
        # That's 6 closing parens - but let-values needs to close before let*
        # Actually: cfg(1), let all-nodes(2), let base-nodes(3), let-values(4), let*(5), define(6)
        # So 6 closing parens is correct, but we need an extra ) for let-values body
        # Wait, let-values has its body in the same expression...
        # Let me check: (let-values ... (let ... (let ... (cfg ...))))
        # So: define(1), let*(2), let-values(3), let base(4), let all(5), cfg(6)
        # Closing: ) for cfg, ) for let all, ) for let base, ) for let-values, ) for let*, ) for define
        # That's 6, which matches. But maybe the issue is that let-values needs to close differently?
        pass
    
    i += 1

with open('racket-metrics/cfg-builder.rkt', 'w') as f:
    f.write('\n'.join(fixed_lines))
    
print("File checked")
