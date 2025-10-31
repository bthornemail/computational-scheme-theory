# Four-Layer Architecture Integration Strategy

**Status:** Planning Phase
**Purpose:** Define how the mathematical core, metrics calculator, and validation framework integrate into the complete CSTP system

---

## 1. Architecture Overview

The complete system follows the mandated four-layer architecture with FSM + Event Sourcing at its core:

```
┌──────────────────────────────────────────────────────────────┐
│ LAYER 1: USER INTERFACE (Observational Layer)                │
│ • Natural Language Interface (Python + symbolic parser)       │
│ • CLI/Web Interface for validation experiments               │
│ • M-Expression dispatch to Layer 4                           │
└───────────────────────┬──────────────────────────────────────┘
                        │ M-Expressions (Commands)
                        ↓
┌──────────────────────────────────────────────────────────────┐
│ LAYER 2: QUERY INTERFACE (Read-Only Topological Layer)       │
│ • Materialized views of validation results                   │
│ • GraphQL API for querying H¹/V(G) results                  │
│ • Read from event stream projections                         │
└───────────────────────┬──────────────────────────────────────┘
                        ↑ State Updates (Read-Only)
┌──────────────────────────────────────────────────────────────┐
│ LAYER 3: COORDINATION (Distributed Layer)                    │
│ • Kafka/Redis Pub/Sub for S-Expression events               │
│ • Vector clocks for causal consistency                       │
│ • Broadcast validation results to subscribers                │
└───────────────────────┬──────────────────────────────────────┘
                        ↑ S-Expressions (Events)
┌──────────────────────────────────────────────────────────────┐
│ LAYER 4: MATHEMATICAL CORE (Algebraic Layer)                 │
│ • FSM validates M-Expressions                                │
│ • Event Sourcing (PostgreSQL) - immutable S-expression log  │
│ • H¹ Calculator (Haskell)                                    │
│ • V(G) Calculator (Racket)                                   │
│ • Validation Coordinator (Python)                            │
└──────────────────────────────────────────────────────────────┘
```

---

## 2. Phase-Based Implementation Strategy

We implement the system in phases, starting with the mathematical core for immediate validation, then expanding to the full architecture.

### Phase 1: Core Validation (Months 1-4) - **MINIMAL VIABLE PRODUCT**

**Goal**: Validate H¹ = V(G) hypothesis without full infrastructure

**Components**:
- Haskell H¹ calculator (standalone)
- Racket V(G) calculator (standalone)
- Python validation coordinator (simple orchestration)
- Test corpus (first 50 programs)
- **NO** event sourcing, pub/sub, or NLI yet

**Architecture** (Simplified):

```
┌─────────────────────────────────────────────┐
│   Python Validation Coordinator (CLI)       │
│   • Load test programs                      │
│   • Call H¹ and V(G) services via gRPC     │
│   • Compare results                         │
│   • Generate report                         │
└──────────┬──────────────────┬───────────────┘
           │                  │
           ↓                  ↓
    ┌──────────────┐   ┌──────────────┐
    │   Haskell    │   │   Racket     │
    │ H¹ Service   │   │ V(G) Service │
    └──────────────┘   └──────────────┘
```

**Deliverables**:
1. Empirical validation results for 50 programs
2. Correlation analysis (H¹ vs V(G))
3. Failure mode identification
4. Conference paper draft

**Success Criteria**:
- All 50 programs successfully analyzed
- Correlation > 0.9
- Clear understanding of k-normalization

---

### Phase 2: Event Sourcing + FSM (Months 5-8)

**Goal**: Add event sourcing and FSM for proper state management

**New Components**:
- PostgreSQL event store
- FSM for validation state machine
- S-expression event log

**Architecture**:

```
┌─────────────────────────────────────────────┐
│   Python Coordinator (CLI)                  │
│   • Dispatch M-expressions to FSM           │
└──────────────────┬──────────────────────────┘
                   │ M-Expressions
                   ↓
┌──────────────────────────────────────────────┐
│   FSM (Python or Haskell)                    │
│   • Validate commands                        │
│   • Append S-expressions to event store     │
│   • Invoke H¹/V(G) calculators              │
└──────────────────┬──────────────────────────┘
                   │ S-Expressions
                   ↓
┌──────────────────────────────────────────────┐
│   PostgreSQL Event Store                     │
│   • Immutable log of validation events      │
└──────────────────────────────────────────────┘
```

**Event Types** (S-expressions):

```scheme
;; Validation requested
(validation-requested
  :program-id "SC001"
  :timestamp 1698765432
  :correlation-id "abc-123")

;; H¹ computed
(h1-computed
  :program-id "SC001"
  :h1 2
  :computation-time-ms 42.5
  :timestamp 1698765433)

;; V(G) computed
(vg-computed
  :program-id "SC001"
  :vg 2
  :computation-time-ms 15.2
  :timestamp 1698765433)

;; Hypothesis validated
(hypothesis-validated
  :program-id "SC001"
  :h1 2
  :vg 2
  :k 0
  :holds true
  :timestamp 1698765434)

;; Protocol anomaly (if hypothesis fails)
(protocol-anomaly
  :program-id "CC042"
  :h1 5
  :vg 7
  :expected-k 1
  :actual-difference 2
  :reason "Unknown"
  :timestamp 1698765500)
```

**FSM States**:

```haskell
data ValidationState
  = ValidationRequested ProgramId
  | H1Computing ProgramId
  | VGComputing ProgramId
  | BothCompleted ProgramId H1Result VGResult
  | HypothesisTested ProgramId Bool  -- True if holds
  | Failed ProgramId ErrorDetails
  deriving (Show, Eq)

data ValidationFSM = FSM
  { currentState :: ValidationState
  , eventLog :: [SExpression]
  , vectorClock :: VectorClock
  }
```

**Deliverables**:
1. Full event sourcing implementation
2. Replay capability for auditing
3. 350-program validation with event log
4. Journal paper draft

---

### Phase 3: Pub/Sub + Distributed Coordination (Months 9-12)

**Goal**: Add distributed coordination and real-time streaming

**New Components**:
- Kafka/Redis for pub/sub
- Multiple validation workers (parallel processing)
- Real-time dashboards

**Architecture**:

```
┌─────────────────────────────────────────────┐
│   Web UI (React + GraphQL)                  │
│   • Real-time validation dashboard          │
└──────────────────┬──────────────────────────┘
                   │ GraphQL queries
                   ↓
┌─────────────────────────────────────────────┐
│   Query Service (Layer 2)                   │
│   • Materialized views                      │
│   • Subscribe to event stream               │
└──────────────────┬──────────────────────────┘
                   ↑ S-Expressions
┌─────────────────────────────────────────────┐
│   Kafka/Redis (Layer 3)                     │
│   • Broadcast validation events             │
│   • Topic: validation-results               │
└──────────────────┬──────────────────────────┘
                   ↑ S-Expressions
┌─────────────────────────────────────────────┐
│   FSM + Event Store (Layer 4)               │
│   • Parallel validation workers (N replicas)│
│   • Distributed via vector clocks           │
└─────────────────────────────────────────────┘
```

**Deliverables**:
1. Real-time validation dashboard
2. Parallel processing of test corpus
3. Distributed system metrics (throughput, latency)
4. Production-ready system

---

### Phase 4: Natural Language Interface (Months 13-16)

**Goal**: Add symbolic NLI for querying validation results

**New Components**:
- Symbolic grammar parser (Python)
- Knowledge graph (Neo4j)
- NLI API (Layer 1)

**Example Queries**:

```
User: "Show me all programs where H¹ doesn't equal V(G)"

NLI: Parse → Query → Respond
  → (query (filter (programs) (not (= h1 vg))))
  → [List of programs with anomalies]

User: "What's the average cyclomatic complexity for recursive programs?"

NLI: Parse → Query → Respond
  → (query (mean (map programs (filter (has-feature 'recursion)) vg)))
  → "Average V(G) for recursive programs: 8.3"

User: "Find programs with H¹ greater than 10"

NLI: Parse → Query → Respond
  → (query (filter (programs) (> h1 10)))
  → [List of 23 programs]
```

**Deliverables**:
1. Symbolic NLI implementation
2. Knowledge graph of validation results
3. User studies on NLI usability
4. PhD thesis chapter

---

## 3. Component Integration Details

### 3.1 Haskell Mathematical Core Integration

**Service Interface**:

```haskell
-- Mathematical Core exposes gRPC service
-- Receives: SchemeProgram
-- Returns: CohomologyResult (including H¹)

main :: IO ()
main = do
  -- Start gRPC server
  runServer (ServerConfig "localhost" 50051) mathCoreService

  -- Listen for commands from FSM
  forever $ do
    request <- receiveRequest
    result <- computeH1 (programSource request)
    sendResponse result
```

**Event Emission**:

```haskell
-- After computing H¹, emit S-expression event
emitEvent :: ProgramId -> H1Result -> IO ()
emitEvent progId result = do
  let event = SExpr
        [ Symbol "h1-computed"
        , Keyword ":program-id" `Pair` String progId
        , Keyword ":h1" `Pair` Number (h1 result)
        , Keyword ":timestamp" `Pair` Number (currentTime)
        ]
  appendToEventStore event
  publishToKafka "validation-events" event
```

### 3.2 Racket Metrics Calculator Integration

**Service Interface**:

```racket
;; Racket V(G) service
;; Receives: SchemeProgram
;; Returns: MetricsResult (including V(G))

(define (start-service port)
  (grpc-serve port
    (hash 'ComputeVG compute-vg-handler)))

(define (compute-vg-handler request)
  ;; Parse → CFG → V(G)
  (define ast (parse-r5rs (proto-ref request 'source_code)))
  (define cfg (build-cfg ast))
  (define metrics (compute-cyclomatic-complexity cfg))

  ;; Emit event
  (emit-vg-event (proto-ref request 'program_id) metrics)

  ;; Return result
  (make-metrics-result metrics))
```

**Event Emission**:

```racket
;; Emit S-expression event for V(G) computation
(define (emit-vg-event program-id metrics)
  (define event
    `(vg-computed
      :program-id ,program-id
      :vg ,(complexity-metrics-v-g metrics)
      :timestamp ,(current-seconds)))

  (append-to-event-store event)
  (publish-to-kafka "validation-events" event))
```

### 3.3 Python Coordinator Integration

**FSM Implementation**:

```python
# coordinator/fsm.py
from enum import Enum
from dataclasses import dataclass
from typing import Optional

class State(Enum):
    IDLE = "idle"
    VALIDATION_REQUESTED = "validation_requested"
    H1_COMPUTING = "h1_computing"
    VG_COMPUTING = "vg_computing"
    BOTH_COMPLETED = "both_completed"
    HYPOTHESIS_TESTED = "hypothesis_tested"
    FAILED = "failed"

@dataclass
class ValidationFSM:
    program_id: str
    state: State
    h1_result: Optional[int] = None
    vg_result: Optional[int] = None
    error: Optional[str] = None
    vector_clock: dict = None

    def __post_init__(self):
        if self.vector_clock is None:
            self.vector_clock = {"coordinator": 0, "haskell": 0, "racket": 0}

    def transition(self, event: dict) -> 'ValidationFSM':
        """Process event and transition to new state"""
        event_type = event['type']

        if event_type == 'validation_requested':
            if self.state == State.IDLE:
                return self._to_validation_requested(event)

        elif event_type == 'h1_computed':
            if self.state == State.VALIDATION_REQUESTED:
                return self._to_h1_computing(event)

        elif event_type == 'vg_computed':
            if self.state == State.H1_COMPUTING:
                return self._to_both_completed(event)

        elif event_type == 'hypothesis_validated':
            if self.state == State.BOTH_COMPLETED:
                return self._to_hypothesis_tested(event)

        else:
            return self._to_failed(f"Invalid transition: {event_type} in {self.state}")

    def _to_validation_requested(self, event):
        self.state = State.VALIDATION_REQUESTED
        self.vector_clock['coordinator'] += 1
        self._append_event(event)
        return self

    # ... other transition methods
```

**Event Store**:

```python
# coordinator/event_store.py
import psycopg2
from datetime import datetime

class PostgresEventStore:
    def __init__(self, connection_string):
        self.conn = psycopg2.connect(connection_string)

    def append(self, event: dict):
        """Append immutable event to log"""
        with self.conn.cursor() as cur:
            cur.execute("""
                INSERT INTO events (
                    event_id, event_type, program_id,
                    payload, timestamp, vector_clock
                ) VALUES (
                    %s, %s, %s, %s, %s, %s
                )
            """, (
                event['event_id'],
                event['type'],
                event['program_id'],
                json.dumps(event['payload']),
                datetime.utcnow(),
                json.dumps(event['vector_clock'])
            ))
            self.conn.commit()

    def replay(self, program_id: str) -> list:
        """Replay all events for a program"""
        with self.conn.cursor() as cur:
            cur.execute("""
                SELECT event_type, payload, timestamp
                FROM events
                WHERE program_id = %s
                ORDER BY timestamp ASC
            """, (program_id,))
            return cur.fetchall()
```

---

## 4. Data Flow Example: Single Validation

### Step-by-step execution for program "SC001":

1. **User submits M-expression** (Layer 1):
   ```scheme
   (validate-program :id "SC001" :source "(if (> x 0) 1 -1)")
   ```

2. **Coordinator receives command** (Layer 4):
   ```python
   fsm = ValidationFSM(program_id="SC001", state=State.IDLE)
   fsm.transition({'type': 'validation_requested', 'program_id': 'SC001'})
   ```

3. **FSM appends event to store**:
   ```sql
   INSERT INTO events VALUES (
     'evt-001', 'validation_requested', 'SC001',
     '{"source": "(if (> x 0) 1 -1)"}', NOW(), '{"coordinator": 1}'
   );
   ```

4. **Coordinator calls H¹ service** (gRPC):
   ```python
   h1_result = math_core_stub.ComputeCohomology(
       CohomologyRequest(program=SchemeProgram(program_id="SC001", ...))
   )
   ```

5. **Haskell computes H¹** → returns 2

6. **H¹ event emitted**:
   ```sql
   INSERT INTO events VALUES (
     'evt-002', 'h1_computed', 'SC001',
     '{"h1": 2, "time_ms": 42.5}', NOW(), '{"coordinator": 1, "haskell": 1}'
   );
   ```

7. **Coordinator calls V(G) service** (gRPC):
   ```python
   vg_result = metrics_stub.ComputeVG(
       MetricsRequest(program=SchemeProgram(program_id="SC001", ...))
   )
   ```

8. **Racket computes V(G)** → returns 2

9. **V(G) event emitted**:
   ```sql
   INSERT INTO events VALUES (
     'evt-003', 'vg_computed', 'SC001',
     '{"vg": 2, "time_ms": 15.2}', NOW(), '{"coordinator": 1, "haskell": 1, "racket": 1}'
   );
   ```

10. **Coordinator tests hypothesis**:
    ```python
    h1 = 2
    vg = 2
    k = 0
    hypothesis_holds = (h1 == vg - k)  # True
    ```

11. **Hypothesis event emitted**:
    ```sql
    INSERT INTO events VALUES (
      'evt-004', 'hypothesis_validated', 'SC001',
      '{"h1": 2, "vg": 2, "k": 0, "holds": true}', NOW(), '{"coordinator": 2, "haskell": 1, "racket": 1}'
    );
    ```

12. **Event published to Kafka** (Layer 3):
    ```python
    producer.send('validation-events', event)
    ```

13. **Query service updates materialized view** (Layer 2):
    ```python
    # Subscriber receives event
    def handle_hypothesis_validated(event):
        db.execute("""
            INSERT INTO validation_results (program_id, h1, vg, holds)
            VALUES (%s, %s, %s, %s)
        """, (event['program_id'], event['h1'], event['vg'], event['holds']))
    ```

14. **User queries result** (Layer 1):
    ```graphql
    query {
      validationResult(programId: "SC001") {
        h1
        vg
        hypothesisHolds
      }
    }
    ```

---

## 5. Deployment Architecture

### 5.1 Development (Local)

```bash
# docker-compose-dev.yml
version: '3.8'

services:
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: cstp_events
      POSTGRES_USER: cstp
      POSTGRES_PASSWORD: dev123
    ports:
      - "5432:5432"

  redis:
    image: redis:7
    ports:
      - "6379:6379"

  math-core:
    build: ./haskell-core
    ports:
      - "50051:50051"
    environment:
      - LOG_LEVEL=DEBUG

  metrics-calc:
    build: ./racket-metrics
    ports:
      - "50052:50052"

  coordinator:
    build: ./python-coordinator
    ports:
      - "50053:50053"
    environment:
      - POSTGRES_URL=postgresql://cstp:dev123@postgres:5432/cstp_events
      - REDIS_URL=redis://redis:6379
      - MATH_CORE_URL=math-core:50051
      - METRICS_URL=metrics-calc:50052
    depends_on:
      - postgres
      - redis
      - math-core
      - metrics-calc
```

### 5.2 Production (Kubernetes)

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: math-core
spec:
  replicas: 3  # Scale for throughput
  selector:
    matchLabels:
      app: math-core
  template:
    metadata:
      labels:
        app: math-core
    spec:
      containers:
      - name: math-core
        image: cstp/math-core:v1.0
        ports:
        - containerPort: 50051
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
          limits:
            memory: "4Gi"
            cpu: "2000m"
        livenessProbe:
          exec:
            command: ["/bin/grpc_health_probe", "-addr=:50051"]
          initialDelaySeconds: 10
          periodSeconds: 10
---
# Similar deployments for metrics-calc, coordinator, etc.
```

---

## 6. Monitoring & Observability

### 6.1 Metrics (Prometheus)

```python
# coordinator/metrics.py
from prometheus_client import Counter, Histogram, Gauge

validations_total = Counter(
    'cstp_validations_total',
    'Total number of validations',
    ['category']
)

h1_computation_duration = Histogram(
    'cstp_h1_computation_seconds',
    'Time to compute H¹',
    buckets=[0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0]
)

hypothesis_holds_total = Counter(
    'cstp_hypothesis_holds_total',
    'Number of times hypothesis holds',
    ['k_value']
)

event_store_size = Gauge(
    'cstp_event_store_size_bytes',
    'Size of event store'
)
```

### 6.2 Distributed Tracing (OpenTelemetry)

```python
from opentelemetry import trace
from opentelemetry.sdk.trace import TracerProvider

tracer = trace.get_tracer(__name__)

def validate_program(program_id):
    with tracer.start_as_current_span("validate_program") as span:
        span.set_attribute("program.id", program_id)

        with tracer.start_as_current_span("compute_h1"):
            h1 = call_h1_service(program_id)

        with tracer.start_as_current_span("compute_vg"):
            vg = call_vg_service(program_id)

        span.set_attribute("result.h1", h1)
        span.set_attribute("result.vg", vg)
```

---

## 7. Testing Strategy

### 7.1 Unit Tests

- Each service has comprehensive unit tests
- Haskell: QuickCheck properties
- Racket: rackunit tests
- Python: pytest

### 7.2 Integration Tests

```python
# tests/test_integration.py
def test_end_to_end_validation():
    """Test complete validation flow"""
    # Given: A simple program
    program = SchemeProgram(
        program_id="test-001",
        source_code="(if (> x 0) 1 -1)"
    )

    # When: We request validation
    result = coordinator.validate_program(program)

    # Then: We get correct results
    assert result.h1 == 2
    assert result.vg == 2
    assert result.hypothesis_holds == True

    # And: Events are in store
    events = event_store.replay("test-001")
    assert len(events) == 4  # requested, h1, vg, validated
```

### 7.3 Performance Tests

```python
# tests/test_performance.py
def test_throughput():
    """Validate 100 programs in under 10 seconds"""
    programs = load_test_corpus(100)

    start = time.time()
    results = coordinator.validate_corpus(programs, parallel=True)
    elapsed = time.time() - start

    assert elapsed < 10.0
    assert len(results) == 100
```

---

## 8. Next Steps

1. **Phase 1 MVP** (Start immediately)
   - Implement standalone services
   - Run first 50-program validation
   - Publish initial results

2. **Phase 2 Event Sourcing** (After Phase 1 validates hypothesis)
   - Add PostgreSQL event store
   - Implement FSM
   - Add event replay capability

3. **Phase 3 Distribution** (After Phase 2 stabilizes)
   - Add Kafka/Redis
   - Implement parallel processing
   - Deploy on Kubernetes

4. **Phase 4 NLI** (Final phase)
   - Build symbolic parser
   - Add knowledge graph
   - User studies

**Estimated Timeline**: 16 months total
- Phase 1: 4 months
- Phase 2: 4 months
- Phase 3: 4 months
- Phase 4: 4 months
