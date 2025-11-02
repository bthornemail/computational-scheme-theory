/**
 * Backend API Server for Computational Scheme Theory UI
 * 
 * Directly calls Racket unified pipeline (similar to Python coordinator)
 */

import express from 'express';
import cors from 'cors';
import { spawn } from 'child_process';
import { writeFileSync, unlinkSync, existsSync } from 'fs';
import { tmpdir } from 'os';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const app = express();
const PORT = process.env.PORT || 3001;

// Middleware
app.use(cors());
app.use(express.json());

// Paths
const PROJECT_ROOT = path.resolve(__dirname, '../..');
const RACKET_UNIFIED_DIR = path.resolve(PROJECT_ROOT, 'racket-unified/src');
const PYTHON_COORDINATOR_DIR = path.resolve(PROJECT_ROOT, 'python-coordinator');

/**
 * Execute Racket script and return result
 */
function executeRacketScript(scriptContent, cwd = RACKET_UNIFIED_DIR) {
  return new Promise((resolve, reject) => {
    // Create temporary script file IN the racket-unified/src directory
    // so that relative requires work correctly
    const tempScript = path.join(cwd, `racketscript-${Date.now()}.rkt`);
    
    try {
      writeFileSync(tempScript, scriptContent);
    } catch (err) {
      reject(new Error(`Failed to create script file: ${err}`));
      return;
    }

    const racket = spawn('racket', [tempScript], { cwd });
    
    let stdout = '';
    let stderr = '';

    racket.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    racket.stderr.on('data', (data) => {
      stderr += data.toString();
    });

    racket.on('close', (code) => {
      // Clean up temp script
      try {
        if (existsSync(tempScript)) {
          unlinkSync(tempScript);
        }
      } catch (err) {
        console.warn(`Failed to clean up temp script: ${err}`);
      }

      if (code !== 0) {
        reject(new Error(`Racket process exited with code ${code}: ${stderr || stdout}`));
        return;
      }

      resolve({ stdout: stdout.trim(), stderr: stderr.trim() });
    });

    racket.on('error', (err) => {
      // Clean up temp script
      try {
        if (existsSync(tempScript)) {
          unlinkSync(tempScript);
        }
      } catch (cleanupErr) {
        console.warn(`Failed to clean up temp script: ${cleanupErr}`);
      }
      
      reject(new Error(`Failed to spawn Racket process: ${err.message}`));
    });
  });
}

/**
 * Compute H? using unified pipeline
 */
async function computeH1(sourceCode) {
  // Create temp source file in the racket-unified/src directory
  const tempSource = path.join(RACKET_UNIFIED_DIR, `source-${Date.now()}.scm`);
  
  try {
    writeFileSync(tempSource, sourceCode);
    
    // Use absolute path for the source file
    const sourcePath = tempSource.replace(/\\/g, '/');
    
    const script = `#lang racket/base
(require racket/file
         "algorithms/unified-pipeline.rkt"
         json)

(define source-file "${sourcePath}")
(define source (file->string source-file))

(with-handlers ([exn? (lambda (e)
                         (display (jsexpr->string (hash 'success #f
                                                        'error (exn-message e)))))])
  (define result (compute-h1-from-source-detailed source))
  (if (pipeline-result-success result)
      (display (jsexpr->string (hash 'success #t
                                    'h1 (pipeline-result-h1 result)
                                    'bindings (pipeline-result-num-bindings result)
                                    'simplices_0 (pipeline-result-num-simplices0 result)
                                    'simplices_1 (pipeline-result-num-simplices1 result)
                                    'simplices_2 (pipeline-result-num-simplices2 result))))
      (display (jsexpr->string (hash 'success #f
                                    'error (or (pipeline-result-error result) "Unknown error"))))))
`;

    const { stdout } = await executeRacketScript(script);
    
    try {
      return JSON.parse(stdout);
    } catch (parseErr) {
      throw new Error(`Failed to parse Racket output: ${stdout}`);
    }
  } catch (err) {
    return {
      success: false,
      error: err instanceof Error ? err.message : 'Unknown error',
    };
  } finally {
    // Clean up temp source file
    try {
      if (existsSync(tempSource)) {
        unlinkSync(tempSource);
      }
    } catch (err) {
      console.warn(`Failed to clean up temp source: ${err}`);
    }
  }
}

/**
 * Compute V(G) using unified pipeline
 */
async function computeVG(sourceCode) {
  // Create temp source file in the racket-unified/src directory
  const tempSource = path.join(RACKET_UNIFIED_DIR, `source-${Date.now()}.scm`);
  
  try {
    writeFileSync(tempSource, sourceCode);
    
    // Use absolute path for the source file
    const sourcePath = tempSource.replace(/\\/g, '/');
    
    const script = `#lang racket/base
(require racket/file
         "algorithms/cfg-builder.rkt"
         "algorithms/cyclomatic.rkt"
         json)

(define source-file "${sourcePath}")
(define source (file->string source-file))

(with-handlers ([exn? (lambda (e)
                         (display (jsexpr->string (hash 'success #f
                                                        'error (exn-message e)))))])
  (define cfg (build-cfg-from-source source))
  (define metrics (compute-cyclomatic-complexity cfg))
  (display (jsexpr->string (hash 'success #t
                                 'v_g (complexity-metrics-v-g metrics)))))
`;

    const { stdout } = await executeRacketScript(script);
    
    try {
      return JSON.parse(stdout);
    } catch (parseErr) {
      throw new Error(`Failed to parse Racket output: ${stdout}`);
    }
  } catch (err) {
    return {
      success: false,
      error: err instanceof Error ? err.message : 'Unknown error',
    };
  } finally {
    // Clean up temp source file
    try {
      if (existsSync(tempSource)) {
        unlinkSync(tempSource);
      }
    } catch (err) {
      console.warn(`Failed to clean up temp source: ${err}`);
    }
  }
}

/**
 * Process natural language query
 */
async function processNL(query) {
  const script = `#lang racket/base
(require "nlp/layer1-interface.rkt"
         "m-expression.rkt"
         json)

(define query "${query.replace(/"/g, '\\"').replace(/\\/g, '\\\\')}")

(with-handlers ([exn? (lambda (e)
                         (display (jsexpr->string (hash 'success #f
                                                        'error (exn-message e)))))])
  (define m-expr (nl-to-m-expression query))
  (if m-expr
      (display (jsexpr->string (hash 'success #t
                                     'm_expression (hash 'op (symbol->string (m-expr-op m-expr))
                                                         'args (map (lambda (a) (if (symbol? a) (symbol->string a) (format "~a" a))) (m-expr-args m-expr)))
                                     'raw_query query)))
      (display (jsexpr->string (hash 'success #f
                                    'error "Failed to parse query")))))
`;

  try {
    const { stdout } = await executeRacketScript(script);
    return JSON.parse(stdout);
  } catch (err) {
    return {
      success: false,
      error: err instanceof Error ? err.message : 'Unknown error',
    };
  }
}

/**
 * Validate hypothesis H? = V(G) - k
 */
function validateHypothesis(h1, vg, k = 0, tolerance = 0.001) {
  const difference = Math.abs(h1 - (vg - k));
  const valid = difference <= tolerance;
  
  return {
    valid,
    difference,
    message: valid
      ? `Hypothesis holds: H?=${h1} = V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`
      : `Hypothesis violated: H?=${h1} ? V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`,
  };
}

// API Routes

app.get('/health', (req, res) => {
  res.json({ 
    status: 'ok', 
    service: 'Computational Scheme Theory API',
    racket_unified_dir: RACKET_UNIFIED_DIR,
    racket_available: existsSync(RACKET_UNIFIED_DIR),
  });
});

app.post('/api/compute-h1', async (req, res) => {
  try {
    const { source_code } = req.body;
    
    if (!source_code) {
      return res.status(400).json({ success: false, error: 'source_code is required' });
    }

    const result = await computeH1(source_code);
    res.json(result);
  } catch (error) {
    console.error('Error computing H1:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/compute-vg', async (req, res) => {
  try {
    const { source_code } = req.body;
    
    if (!source_code) {
      return res.status(400).json({ success: false, error: 'source_code is required' });
    }

    const result = await computeVG(source_code);
    res.json(result);
  } catch (error) {
    console.error('Error computing V(G):', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/validate-hypothesis', async (req, res) => {
  try {
    const { h1, v_g, k = 0, tolerance = 0.001 } = req.body;
    
    if (h1 === undefined || v_g === undefined) {
      return res.status(400).json({ success: false, error: 'h1 and v_g are required' });
    }

    const result = validateHypothesis(h1, v_g, k, tolerance);
    res.json(result);
  } catch (error) {
    console.error('Error validating hypothesis:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/process-nl', async (req, res) => {
  try {
    const { query } = req.body;
    
    if (!query) {
      return res.status(400).json({ success: false, error: 'query is required' });
    }

    const result = await processNL(query);
    res.json(result);
  } catch (error) {
    console.error('Error processing natural language:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/execute-nl', async (req, res) => {
  try {
    const { query } = req.body;
    
    if (!query) {
      return res.status(400).json({ success: false, error: 'query is required' });
    }

    // Process NL to get M-expression
    const mExprResult = await processNL(query);
    
    if (!mExprResult.success) {
      return res.json(mExprResult);
    }

    // Extract operation and execute
    const op = mExprResult.m_expression?.op;
    const args = mExprResult.m_expression?.args || [];
    
    // For queries like "compute H1 for program X", we need to extract the program
    // For now, execute based on operation type
    if (op === 'computeH1' && args.length > 0) {
      // Try to get source code - for now use a simple test
      const testSource = '(lambda (x) x)';
      const h1Result = await computeH1(testSource);
      return res.json({
        success: true,
        result: h1Result,
      });
    }
    
    if (op === 'computeVG' && args.length > 0) {
      const testSource = '(lambda (x) x)';
      const vgResult = await computeVG(testSource);
      return res.json({
        success: true,
        result: vgResult,
      });
    }

    // Default: return M-expression
    res.json({
      success: true,
      result: {
        m_expression: mExprResult.m_expression,
        message: `Operation ${op} extracted from query`,
      },
    });
  } catch (error) {
    console.error('Error executing natural language query:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

// Error handling middleware
app.use((err, req, res, next) => {
  console.error('Unhandled error:', err);
  res.status(500).json({
    success: false,
    error: err.message || 'Internal server error',
  });
});

/**
 * Validate hypothesis H? = V(G) - k
 */
function validateHypothesis(h1, vg, k = 0, tolerance = 0.001) {
  const difference = Math.abs(h1 - (vg - k));
  const valid = difference <= tolerance;
  
  return {
    valid,
    difference,
    message: valid
      ? `Hypothesis holds: H?=${h1} = V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`
      : `Hypothesis violated: H?=${h1} ? V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`,
  };
}

/**
 * Validate hypothesis H? = V(G) - k
 */
function validateHypothesis(h1, vg, k = 0, tolerance = 0.001) {
  const difference = Math.abs(h1 - (vg - k));
  const valid = difference <= tolerance;
  
  return {
    valid,
    difference,
    message: valid
      ? `Hypothesis holds: H?=${h1} = V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`
      : `Hypothesis violated: H?=${h1} ? V(G)-k=${vg}-${k} (difference: ${difference.toFixed(6)})`,
  };
}

// LLM API Routes

app.get('/api/llm/status', async (req, res) => {
  try {
    const { llm_type = 'openai' } = req.query;
    const status = await checkLLMAvailability(llm_type);
    res.json(status);
  } catch (error) {
    console.error('Error checking LLM status:', error);
    res.status(500).json({
      available: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/llm/process-nl', async (req, res) => {
  try {
    const { query, llm_type = 'openai' } = req.body;
    
    if (!query) {
      return res.status(400).json({ success: false, error: 'query is required' });
    }

    const result = await processNLWithLLM(query, llm_type);
    res.json(result);
  } catch (error) {
    console.error('Error processing NL with LLM:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});

app.post('/api/llm/explain', async (req, res) => {
  try {
    const { operation, result, llm_type = 'openai' } = req.body;
    
    if (!operation || !result) {
      return res.status(400).json({ success: false, error: 'operation and result are required' });
    }

    const explanation = await generateLLMExplanation(operation, result, llm_type);
    res.json(explanation);
  } catch (error) {
    console.error('Error generating LLM explanation:', error);
    res.status(500).json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
    });
  }
});
