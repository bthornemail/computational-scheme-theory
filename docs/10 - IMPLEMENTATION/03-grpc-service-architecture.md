# gRPC Service Architecture Design

**Status:** Planning Phase
**Purpose:** Define service contracts and communication patterns between components

---

## 1. Service Overview

The system consists of three primary computational services:

1. **Mathematical Core Service** (Haskell) - Computes H¹(X_Comp, O_Comp)
2. **Metrics Calculator Service** (Racket) - Computes V(G) from CFG
3. **Coordinator Service** (Python) - Orchestrates validation experiments

```
┌─────────────────────────────────────────────────────────┐
│                    Python Coordinator                    │
│         (Validation Orchestrator + Test Harness)        │
└───────────┬─────────────────────────────┬───────────────┘
            │                             │
            │ gRPC                        │ gRPC
            ↓                             ↓
┌───────────────────────┐    ┌───────────────────────────┐
│   Haskell Math Core   │    │   Racket Metrics Calc     │
│   (H¹ Calculator)     │    │   (V(G) Calculator)       │
└───────────────────────┘    └───────────────────────────┘
```

---

## 2. Protocol Buffer Definitions

### 2.1 Common Types

```protobuf
// proto/common.proto
syntax = "proto3";

package computational_scheme.common;

// Empty message for RPCs that need no input
message Empty {}

// Health check response
message HealthStatus {
  bool healthy = 1;
  string version = 2;
  string service_name = 3;
  int64 uptime_seconds = 4;
}

// Source location in program
message SourceLocation {
  string file = 1;
  int32 line = 2;
  int32 column = 3;
  int32 start_pos = 4;
  int32 end_pos = 5;
}

// A Scheme program to analyze
message SchemeProgram {
  string program_id = 1;        // Unique identifier
  string source_code = 2;       // R5RS Scheme source
  string description = 3;       // Human-readable description
  map<string, string> metadata = 4;  // Additional info
}

// Error details
message ErrorDetails {
  string error_code = 1;        // Machine-readable error code
  string message = 2;           // Human-readable message
  SourceLocation location = 3;  // Where error occurred (if applicable)
  repeated string stack_trace = 4;  // Stack trace for debugging
}
```

### 2.2 Mathematical Core Service

```protobuf
// proto/math_core.proto
syntax = "proto3";

package computational_scheme.math_core;

import "common.proto";

service MathematicalCore {
  // Compute full cohomology analysis
  rpc ComputeCohomology(CohomologyRequest) returns (CohomologyResult);

  // Extract binding algebra only (for debugging)
  rpc ExtractBindingAlgebra(common.SchemeProgram) returns (BindingAlgebraResult);

  // Extract scope topology only (for debugging)
  rpc ExtractScopeTopology(common.SchemeProgram) returns (ScopeTopologyResult);

  // Build Čech complex only (for debugging)
  rpc BuildCechComplex(common.SchemeProgram) returns (CechComplexResult);

  // Health check
  rpc HealthCheck(common.Empty) returns (common.HealthStatus);
}

// Request for cohomology computation
message CohomologyRequest {
  common.SchemeProgram program = 1;
  bool include_debug_info = 2;    // Include intermediate results
  int32 max_dimension = 3;         // Compute up to H^n (0 = auto)
}

// Result of cohomology computation
message CohomologyResult {
  string program_id = 1;

  // Betti numbers (primary result)
  int32 beta_0 = 2;  // Connected components
  int32 beta_1 = 3;  // 1-dimensional holes (cycles) - OUR TARGET
  int32 beta_2 = 4;  // 2-dimensional holes
  repeated int32 beta_higher = 5;  // Higher Betti numbers

  // Computation metrics
  int32 num_bindings = 6;
  int32 num_simplices_0 = 7;  // Vertices
  int32 num_simplices_1 = 8;  // Edges
  int32 num_simplices_2 = 9;  // Triangles
  double computation_time_ms = 10;

  // Debug information (if requested)
  BindingAlgebraResult binding_algebra = 11;
  ScopeTopologyResult scope_topology = 12;
  CechComplexResult cech_complex = 13;

  // Error handling
  common.ErrorDetails error = 14;
  bool success = 15;
}

// Binding algebra (R_Scheme)
message BindingAlgebraResult {
  string program_id = 1;
  repeated Binding bindings = 2;
  bool success = 3;
  common.ErrorDetails error = 4;
}

message Binding {
  string id = 1;                    // Unique ID after α-conversion
  string original_name = 2;         // Original variable name
  BindingType type = 3;             // lambda, let, define, etc.
  common.SourceLocation location = 4;
  repeated ScopeRegion visibility = 5;  // Where this binding is visible
}

enum BindingType {
  BINDING_UNKNOWN = 0;
  BINDING_LAMBDA = 1;
  BINDING_LET = 2;
  BINDING_LETREC = 3;
  BINDING_DEFINE = 4;
  BINDING_PARAMETER = 5;
}

message ScopeRegion {
  int32 start_pos = 1;
  int32 end_pos = 2;
  string description = 3;
}

// Scope topology (Zariski topology)
message ScopeTopologyResult {
  string program_id = 1;
  repeated OpenSet open_sets = 2;  // Collection {D(f)}
  bool success = 3;
  common.ErrorDetails error = 4;
}

message OpenSet {
  string binding_id = 1;           // Which binding f
  repeated ScopeRegion regions = 2;  // Visibility regions D(f)
}

// Čech complex
message CechComplexResult {
  string program_id = 1;
  repeated Simplex simplices_0 = 2;
  repeated Simplex simplices_1 = 3;
  repeated Simplex simplices_2 = 4;
  bool success = 5;
  common.ErrorDetails error = 6;
}

message Simplex {
  repeated int32 vertices = 1;  // Vertex IDs
  int32 dimension = 2;           // 0, 1, 2, ...
}
```

### 2.3 Metrics Calculator Service

```protobuf
// proto/metrics_calc.proto
syntax = "proto3";

package computational_scheme.metrics;

import "common.proto";

service MetricsCalculator {
  // Compute cyclomatic complexity V(G)
  rpc ComputeVG(MetricsRequest) returns (MetricsResult);

  // Build CFG only (for visualization)
  rpc BuildCFG(common.SchemeProgram) returns (CFGResult);

  // Health check
  rpc HealthCheck(common.Empty) returns (common.HealthStatus);
}

message MetricsRequest {
  common.SchemeProgram program = 1;
  bool include_cfg = 2;          // Include CFG in response
  bool include_dot_graph = 3;    // Include Graphviz DOT format
}

message MetricsResult {
  string program_id = 1;

  // Cyclomatic complexity (primary result)
  int32 v_g = 2;

  // CFG statistics
  int32 num_nodes = 3;
  int32 num_edges = 4;
  int32 num_components = 5;  // Connected components (usually 1)

  // Formula used
  string formula = 6;  // "E - N + 2P"

  // Computation metrics
  double computation_time_ms = 7;

  // Debug information (if requested)
  CFGResult cfg = 8;

  // Error handling
  common.ErrorDetails error = 9;
  bool success = 10;
}

// Control Flow Graph
message CFGResult {
  string program_id = 1;
  repeated CFGNode nodes = 2;
  repeated CFGEdge edges = 3;
  int32 entry_node_id = 4;
  int32 exit_node_id = 5;
  string dot_graph = 6;  // Graphviz DOT format
}

message CFGNode {
  int32 id = 1;
  CFGNodeType type = 2;
  repeated string statements = 3;  // Source code snippets
  common.SourceLocation location = 4;
}

enum CFGNodeType {
  NODE_UNKNOWN = 0;
  NODE_ENTRY = 1;
  NODE_EXIT = 2;
  NODE_BASIC = 3;
  NODE_BRANCH = 4;
  NODE_JOIN = 5;
}

message CFGEdge {
  int32 from_node = 1;
  int32 to_node = 2;
  CFGEdgeType type = 3;
  string condition = 4;  // For conditional edges
}

enum CFGEdgeType {
  EDGE_UNKNOWN = 0;
  EDGE_NORMAL = 1;
  EDGE_TRUE_BRANCH = 2;
  EDGE_FALSE_BRANCH = 3;
  EDGE_RETURN = 4;
  EDGE_BACK = 5;  // For loops/recursion
}
```

### 2.4 Coordinator Service

```protobuf
// proto/coordinator.proto
syntax = "proto3";

package computational_scheme.coordinator;

import "common.proto";
import "math_core.proto";
import "metrics_calc.proto";

service ValidationCoordinator {
  // Run validation experiment on a single program
  rpc ValidateProgram(ValidationRequest) returns (ValidationResult);

  // Run validation on entire test corpus
  rpc ValidateCorpus(CorpusRequest) returns (stream CorpusProgress);

  // Get validation statistics
  rpc GetStatistics(StatisticsRequest) returns (StatisticsResult);

  // Health check
  rpc HealthCheck(common.Empty) returns (common.HealthStatus);
}

message ValidationRequest {
  common.SchemeProgram program = 1;
  bool include_debug_info = 2;
}

message ValidationResult {
  string program_id = 1;

  // Primary results
  int32 h1 = 2;  // From Haskell service
  int32 vg = 3;  // From Racket service

  // Hypothesis test
  int32 k = 4;              // Normalization constant
  int32 difference = 5;     // |H¹ - V(G)|
  bool hypothesis_holds = 6;  // Is H¹ = V(G) - k?

  // Full results (if requested)
  math_core.CohomologyResult cohomology = 7;
  metrics.MetricsResult metrics = 8;

  // Timing
  double total_time_ms = 9;
  double h1_time_ms = 10;
  double vg_time_ms = 11;

  // Error handling
  common.ErrorDetails error = 12;
  bool success = 13;
}

message CorpusRequest {
  repeated string program_ids = 1;  // If empty, run all
  string corpus_path = 2;           // Path to test corpus
  bool parallel = 3;                // Run tests in parallel
  int32 max_workers = 4;            // Number of parallel workers
}

message CorpusProgress {
  int32 total_programs = 1;
  int32 completed = 2;
  int32 succeeded = 3;
  int32 failed = 4;
  ValidationResult current_result = 5;  // Most recent result
  double elapsed_seconds = 6;
  double estimated_remaining_seconds = 7;
}

message StatisticsRequest {
  string corpus_run_id = 1;  // Which run to get stats for
}

message StatisticsResult {
  string corpus_run_id = 1;
  int32 total_programs = 2;
  int32 succeeded = 3;
  int32 failed = 4;

  // Hypothesis validation
  int32 hypothesis_holds_count = 5;
  int32 hypothesis_fails_count = 6;
  double hypothesis_success_rate = 7;

  // Difference statistics
  double mean_difference = 8;
  double std_dev_difference = 9;
  double max_difference = 10;
  double min_difference = 11;

  // Timing statistics
  double mean_h1_time_ms = 12;
  double mean_vg_time_ms = 13;
  double total_time_seconds = 14;
}
```

---

## 3. Service Implementation Plan

### 3.1 Haskell Mathematical Core Service

```haskell
-- haskell-core/src/ComputationalScheme/Service/GRPC.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ComputationalScheme.Service.GRPC where

import Network.GRPC.HighLevel.Server
import qualified Proto.MathCore as Proto

-- Service implementation
mathCoreService :: Service MathematicalCore ServerRequest ServerResponse
mathCoreService = Service
  { computeCohomology = computeCohomologyHandler
  , extractBindingAlgebra = extractBindingAlgebraHandler
  , extractScopeTopology = extractScopeTopologyHandler
  , buildCechComplex = buildCechComplexHandler
  , healthCheck = healthCheckHandler
  }

-- Handler implementations
computeCohomologyHandler :: Proto.CohomologyRequest -> IO Proto.CohomologyResult
computeCohomologyHandler req = do
  let program = Proto.program req
      sourceCode = Proto.sourceCode program

  -- Algorithm pipeline: Parse → Rig → Topology → Čech → H¹
  result <- runComputationPipeline sourceCode

  return $ Proto.CohomologyResult
    { programId = Proto.programId program
    , beta0 = result_beta0 result
    , beta1 = result_beta1 result  -- Our target
    , beta2 = result_beta2 result
    , success = True
    , error = Nothing
    }

-- Start server
startServer :: Int -> IO ()
startServer port = do
  let config = ServerConfig
        { host = "localhost"
        , port = port
        , useCompression = True
        }
  runServer config mathCoreService
```

### 3.2 Racket Metrics Calculator Service

```racket
;; racket-metrics/metrics-api.rkt
#lang racket

(require "r5rs-parser.rkt"
         "cfg-builder.rkt"
         "cyclomatic.rkt"
         grpc/server)  ; Hypothetical Racket gRPC library

(provide start-metrics-service)

;; Service handlers
(define (compute-vg-handler request)
  (define source-code (proto-ref request 'source_code))
  (define program-id (proto-ref request 'program_id))

  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-response program-id e))])
    ;; Pipeline: Parse → CFG → V(G)
    (define ast (parse-r5rs source-code))
    (define cfg (build-cfg ast))
    (define metrics (compute-cyclomatic-complexity cfg))

    ;; Build response
    (proto-message 'MetricsResult
      'program_id program-id
      'v_g (complexity-metrics-v-g metrics)
      'num_nodes (complexity-metrics-nodes metrics)
      'num_edges (complexity-metrics-edges metrics)
      'num_components (complexity-metrics-components metrics)
      'formula (complexity-metrics-formula metrics)
      'success #t)))

;; Start server
(define (start-metrics-service port)
  (define handlers
    (hash 'ComputeVG compute-vg-handler
          'BuildCFG build-cfg-handler
          'HealthCheck health-check-handler))

  (grpc-serve port handlers))
```

### 3.3 Python Coordinator Service

```python
# python-coordinator/coordinator_service.py
import grpc
from concurrent import futures
import logging

# Import generated proto code
from proto import coordinator_pb2
from proto import coordinator_pb2_grpc
from proto import math_core_pb2_grpc
from proto import metrics_calc_pb2_grpc

class ValidationCoordinator(coordinator_pb2_grpc.ValidationCoordinatorServicer):
    def __init__(self, math_core_host, metrics_host):
        # Connect to backend services
        self.math_core_channel = grpc.insecure_channel(math_core_host)
        self.math_core_stub = math_core_pb2_grpc.MathematicalCoreStub(
            self.math_core_channel
        )

        self.metrics_channel = grpc.insecure_channel(metrics_host)
        self.metrics_stub = metrics_calc_pb2_grpc.MetricsCalculatorStub(
            self.metrics_channel
        )

    def ValidateProgram(self, request, context):
        """Validate hypothesis for a single program"""
        program = request.program
        logging.info(f"Validating program: {program.program_id}")

        try:
            # Call both services in parallel
            import concurrent.futures
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
                h1_future = executor.submit(self._compute_h1, program)
                vg_future = executor.submit(self._compute_vg, program)

                h1_result = h1_future.result()
                vg_result = vg_future.result()

            # Test hypothesis: H¹ = V(G) - k
            h1 = h1_result.beta_1
            vg = vg_result.v_g
            k = self._estimate_k(program)  # May be 0, 1, or 2
            difference = abs(h1 - (vg - k))
            hypothesis_holds = (difference == 0)

            return coordinator_pb2.ValidationResult(
                program_id=program.program_id,
                h1=h1,
                vg=vg,
                k=k,
                difference=difference,
                hypothesis_holds=hypothesis_holds,
                cohomology=h1_result if request.include_debug_info else None,
                metrics=vg_result if request.include_debug_info else None,
                success=True
            )

        except Exception as e:
            logging.error(f"Validation failed: {e}")
            return coordinator_pb2.ValidationResult(
                program_id=program.program_id,
                success=False,
                error=self._make_error(e)
            )

    def _compute_h1(self, program):
        """Call Haskell math core"""
        request = math_core_pb2.CohomologyRequest(program=program)
        return self.math_core_stub.ComputeCohomology(request)

    def _compute_vg(self, program):
        """Call Racket metrics calculator"""
        request = metrics_calc_pb2.MetricsRequest(program=program)
        return self.metrics_stub.ComputeVG(request)

    def _estimate_k(self, program):
        """Estimate normalization constant k"""
        # For now, try k=1 (most common)
        # TODO: Implement adaptive k selection
        return 1

def serve(port, math_core_host, metrics_host):
    """Start the coordinator service"""
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    coordinator_pb2_grpc.add_ValidationCoordinatorServicer_to_server(
        ValidationCoordinator(math_core_host, metrics_host),
        server
    )
    server.add_insecure_port(f'[::]:{port}')
    server.start()
    logging.info(f"Coordinator service started on port {port}")
    server.wait_for_termination()
```

---

## 4. Service Configuration

### 4.1 Docker Compose (Production)

```yaml
# docker-compose.yml
version: '3.8'

services:
  math-core:
    build:
      context: ./haskell-core
      dockerfile: Dockerfile
    ports:
      - "50051:50051"
    environment:
      - PORT=50051
      - LOG_LEVEL=INFO
    healthcheck:
      test: ["CMD", "grpc_health_probe", "-addr=:50051"]
      interval: 10s
      timeout: 5s
      retries: 3

  metrics-calc:
    build:
      context: ./racket-metrics
      dockerfile: Dockerfile
    ports:
      - "50052:50052"
    environment:
      - PORT=50052
      - LOG_LEVEL=INFO
    healthcheck:
      test: ["CMD", "grpc_health_probe", "-addr=:50052"]
      interval: 10s
      timeout: 5s
      retries: 3

  coordinator:
    build:
      context: ./python-coordinator
      dockerfile: Dockerfile
    ports:
      - "50053:50053"
    environment:
      - PORT=50053
      - MATH_CORE_HOST=math-core:50051
      - METRICS_HOST=metrics-calc:50052
      - LOG_LEVEL=INFO
    depends_on:
      - math-core
      - metrics-calc
    healthcheck:
      test: ["CMD", "grpc_health_probe", "-addr=:50053"]
      interval: 10s
      timeout: 5s
      retries: 3
```

### 4.2 Development Configuration

```bash
# dev-env.sh - Start services locally
#!/bin/bash

# Start Haskell service
cd haskell-core
stack run computational-scheme-server -- --port 50051 &
MATH_PID=$!

# Start Racket service
cd ../racket-metrics
racket metrics-api.rkt --port 50052 &
METRICS_PID=$!

# Start Python coordinator
cd ../python-coordinator
python coordinator_service.py --port 50053 \
  --math-core localhost:50051 \
  --metrics localhost:50052 &
COORD_PID=$!

echo "Services started:"
echo "  Math Core: PID $MATH_PID (port 50051)"
echo "  Metrics: PID $METRICS_PID (port 50052)"
echo "  Coordinator: PID $COORD_PID (port 50053)"

# Cleanup on exit
trap "kill $MATH_PID $METRICS_PID $COORD_PID" EXIT
wait
```

---

## 5. Code Generation

### 5.1 Makefile for Proto Compilation

```makefile
# Makefile
.PHONY: proto clean test

PROTO_DIR = proto
OUT_HASKELL = haskell-core/src/Proto
OUT_RACKET = racket-metrics/proto-gen
OUT_PYTHON = python-coordinator/proto

proto: proto-haskell proto-racket proto-python

proto-haskell:
	mkdir -p $(OUT_HASKELL)
	protoc --proto_path=$(PROTO_DIR) \
	       --haskell_out=$(OUT_HASKELL) \
	       $(PROTO_DIR)/*.proto

proto-racket:
	mkdir -p $(OUT_RACKET)
	# TODO: Find Racket protobuf compiler
	# For now, use JSON over gRPC or manual implementation

proto-python:
	mkdir -p $(OUT_PYTHON)
	python -m grpc_tools.protoc \
	       --proto_path=$(PROTO_DIR) \
	       --python_out=$(OUT_PYTHON) \
	       --grpc_python_out=$(OUT_PYTHON) \
	       $(PROTO_DIR)/*.proto

clean:
	rm -rf $(OUT_HASKELL) $(OUT_RACKET) $(OUT_PYTHON)

test:
	cd haskell-core && stack test
	cd racket-metrics && raco test .
	cd python-coordinator && pytest
```

---

## 6. Testing Strategy

### 6.1 Unit Tests (Per Service)

Each service should have comprehensive unit tests for its handlers.

### 6.2 Integration Tests

```python
# python-coordinator/tests/test_integration.py
import pytest
import grpc
from proto import coordinator_pb2, coordinator_pb2_grpc, common_pb2

@pytest.fixture
def coordinator_stub():
    channel = grpc.insecure_channel('localhost:50053')
    return coordinator_pb2_grpc.ValidationCoordinatorStub(channel)

def test_validate_simple_program(coordinator_stub):
    """Test validation on a simple if statement"""
    program = common_pb2.SchemeProgram(
        program_id="test-001",
        source_code="(if (> x 0) 1 -1)",
        description="Simple if statement"
    )

    request = coordinator_pb2.ValidationRequest(
        program=program,
        include_debug_info=True
    )

    result = coordinator_stub.ValidateProgram(request)

    assert result.success
    assert result.h1 == 2  # Expected H¹
    assert result.vg == 2  # Expected V(G)
    assert result.hypothesis_holds  # H¹ = V(G) - k
```

---

## 7. Monitoring and Observability

### 7.1 Metrics

- **Request latency** (P50, P95, P99)
- **Request rate** (requests/second)
- **Error rate** (errors/second)
- **Service availability** (uptime %)

### 7.2 Logging

Structured logging with correlation IDs:

```json
{
  "timestamp": "2025-10-31T12:00:00Z",
  "level": "INFO",
  "service": "math-core",
  "correlation_id": "abc-123-def",
  "program_id": "test-001",
  "message": "Computing H¹",
  "duration_ms": 42.5
}
```

### 7.3 Tracing

Use OpenTelemetry for distributed tracing across services.

---

## 8. Next Steps

1. **Generate proto code** for all three languages
2. **Implement health checks** in each service
3. **Build basic request/response** handling
4. **Add error handling** and retries
5. **Set up integration tests**
6. **Deploy with Docker Compose**
7. **Add monitoring/observability**

---

## 9. Open Questions

1. **Racket gRPC maturity**: Is there a production-ready gRPC library for Racket?
   - Alternative: JSON over HTTP REST API
   - Alternative: Unix pipes with protobuf serialization

2. **Normalization constant k**: How to determine k automatically?
   - May need machine learning or heuristics
   - Start with k=1 as default

3. **Streaming for large corpora**: Should ValidateCorpus stream results?
   - Implemented as `stream CorpusProgress` in proto
   - Allows monitoring of long-running experiments

4. **Caching**: Should we cache H¹/V(G) results?
   - Yes, with content-addressable keys (hash of source code)
   - Speeds up re-runs and regression testing
