# The Complete FSM-Based Computational Architecture

## Overview: The Categorical Stack

```
┌─────────────────────────────────────────────────────┐
│ Layer 1: USER INTERFACE (Observational/WHY)        │
│ Pattern: Unidirectional Data Flow (UDF) + MVC      │
│ Role: External observers interact with system       │
└─────────────────────────────────────────────────────┘
                        ↓ Actions
┌─────────────────────────────────────────────────────┐
│ Layer 2: QUERY INTERFACE (Topological/HOW)         │
│ Pattern: CQRS Read Side + Materialized Views       │
│ Role: Project FSM state into queryable topology    │
└─────────────────────────────────────────────────────┘
                        ↑ State Updates
┌─────────────────────────────────────────────────────┐
│ Layer 3: COORDINATION (Distributed/WHERE)          │
│ Pattern: Pub/Sub + State Machine Replication       │
│ Role: Distribute FSM transitions across nodes      │
└─────────────────────────────────────────────────────┘
                        ↑ Events
┌─────────────────────────────────────────────────────┐
│ Layer 4: MATHEMATICAL CORE (Algebraic/WHAT)        │
│ Pattern: FSM + Event Sourcing (ES) + CQRS Write    │
│ Role: Deterministic state machine with provenance  │
└─────────────────────────────────────────────────────┘
```

---

## Layer 4: Mathematical Core (FSM + Event Sourcing)

### **The Finite State Machine Definition**

```haskell
-- The core FSM representing R_Scheme computation
data ComputationalFSM = FSM
  { states :: Set State           -- Finite set of states
  , initialState :: State         -- Initial state S₀
  , transitions :: TransitionFn   -- δ: State × Input → State
  , acceptStates :: Set State     -- Final/accepting states
  , stateInvariant :: Invariant   -- Mathematical constraint
  }

-- State = point in Spec(R_Scheme)
data State = State
  { primeIdeal :: PrimeIdeal      -- 𝔭 ⊂ R_Scheme
  , continuation :: Continuation  -- k (rest of computation)
  , localEnv :: Environment       -- R/𝔭 (local bindings)
  , vectorClock :: VectorClock    -- Causal timestamp
  }

-- Transitions = morphisms in computational scheme
data Transition = Transition
  { fromState :: State
  , input :: Event                -- Command/RPC call
  , toState :: State
  , proof :: TransitionProof      -- Verification that transition preserves invariants
  }
```

### **Event Sourcing: Immutable Transition Log**

```haskell
-- Every FSM transition is an event
data Event 
  = BindingCreated Identifier Scope Timestamp
  | ScopeEntered ScopeId Timestamp
  | ScopeExited ScopeId Timestamp
  | ContinuationCaptured Continuation Timestamp
  | RPCCalled NodeId Method Args Timestamp
  | RPCCompleted NodeId Result Timestamp
  
-- Event store = complete provenance
type EventStore = [Event]

-- Reconstruct any state by replaying events
replayEvents :: EventStore -> State -> State
replayEvents events initialState =
  foldl applyEvent initialState events

applyEvent :: State -> Event -> State
applyEvent state event = 
  case event of
    BindingCreated id scope ts ->
      state { localEnv = extendEnv (localEnv state) id scope }
    
    ContinuationCaptured k ts ->
      state { continuation = k }
    
    RPCCalled node method args ts ->
      state { vectorClock = incrementClock (vectorClock state) node }
    
    -- ... other transitions
```

### **CQRS Write Side: Command Validation**

```haskell
-- Commands must be validated before becoming events
data Command
  = CreateBinding Identifier Scope
  | EnterScope ScopeId
  | ExitScope ScopeId
  | CallRPC NodeId Method Args
  
-- Validation ensures FSM determinism
validateCommand :: State -> Command -> Either Error Event
validateCommand currentState cmd =
  case cmd of
    CreateBinding id scope ->
      if validateHygienicBinding currentState id scope
      then Right (BindingCreated id scope (currentTime))
      else Left (HygieneViolation id)
    
    CallRPC node method args ->
      if vectorClockConsistent currentState node
      then Right (RPCCalled node method args (currentTime))
      else Left (CausalityViolation node)

-- Execute validated command
executeCommand :: Command -> StateT ComputationalFSM IO (Either Error Event)
executeCommand cmd = do
  currentState <- get
  case validateCommand currentState cmd of
    Left err -> return (Left err)
    Right event -> do
      let newState = applyEvent currentState event
      put newState
      appendToEventStore event  -- Persist
      return (Right event)
```

### **Mathematical Invariants**

```haskell
-- Invariants that must hold after every transition
data Invariant
  = HygienicIntegrity      -- Sheaf gluing condition
  | CausalConsistency      -- Vector clock ordering
  | CommutativityProperty  -- R_Scheme commutativity
  | PrimeIdealStructure    -- 𝔭 remains prime
  
verifyInvariant :: State -> Invariant -> Bool
verifyInvariant state inv =
  case inv of
    HygienicIntegrity ->
      verifySheafGluing (localEnv state)
    
    CausalConsistency ->
      vectorClockIsMonotonic (vectorClock state)
    
    CommutativityProperty ->
      bindingOrderIndependent (primeIdeal state)
    
    PrimeIdealStructure ->
      isPrime (primeIdeal state)

-- FSM transition only succeeds if invariants preserved
safeTransition :: State -> Event -> Maybe State
safeTransition state event =
  let newState = applyEvent state event
  in if all (verifyInvariant newState) [minBound..maxBound]
     then Just newState
     else Nothing
```

---

## Layer 3: Coordination (State Machine Replication)

### **Distributed FSM via Raft/Paxos-like Consensus**

```python
# state_machine_replication.py
from typing import Set, Dict, Optional
import asyncio

class ReplicatedFSM:
    """
    Replicated Finite State Machine across multiple nodes.
    Uses Raft-like consensus for linearizable state transitions.
    """
    
    def __init__(self, node_id: str, peers: Set[str]):
        self.node_id = node_id
        self.peers = peers
        self.current_state: State = initial_state()
        self.event_log: List[Event] = []
        self.commit_index: int = 0
        
        # Raft-specific
        self.current_term: int = 0
        self.voted_for: Optional[str] = None
        self.leader_id: Optional[str] = None
    
    async def propose_transition(self, command: Command) -> Event:
        """
        Propose a new FSM transition (command).
        Must achieve consensus before applying.
        """
        # 1. Validate command locally
        validation = self.validate_command(command)
        if not validation.valid:
            raise InvalidCommand(validation.error)
        
        # 2. Convert to event (with proof)
        event = command.to_event(self.current_state)
        
        # 3. Append to log (tentative)
        log_entry = LogEntry(
            term=self.current_term,
            event=event,
            index=len(self.event_log)
        )
        self.event_log.append(log_entry)
        
        # 4. Replicate to majority of nodes
        acks = await self.replicate_to_peers(log_entry)
        
        if len(acks) >= (len(self.peers) + 1) // 2:
            # 5. Commit and apply transition
            self.commit_index = log_entry.index
            self.current_state = apply_event(self.current_state, event)
            
            # 6. Notify subscribers (Pub/Sub)
            await self.publish_state_update(self.current_state)
            
            return event
        else:
            # Rollback tentative entry
            self.event_log.pop()
            raise ConsensusTimeout()
    
    async def replicate_to_peers(self, entry: LogEntry) -> Set[str]:
        """Send AppendEntries RPC to all peers"""
        tasks = [
            self.send_append_entries(peer, entry)
            for peer in self.peers
        ]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        return {peer for peer, result in zip(self.peers, results) if result}
```

### **Pub/Sub for State Updates**

```python
# pubsub_coordinator.py
from dataclasses import dataclass
from typing import Callable, List

@dataclass
class StateUpdate:
    """Published when FSM transitions"""
    old_state: State
    new_state: State
    event: Event
    timestamp: int
    node_id: str

class PubSubCoordinator:
    """
    Pub/Sub layer for distributing FSM state updates.
    Implements the "Sheaf Gluing" coordination.
    """
    
    def __init__(self):
        self.subscribers: Dict[str, List[Callable]] = {}
    
    def subscribe(self, topic: str, callback: Callable[[StateUpdate], None]):
        """Subscribe to state updates for a topic"""
        if topic not in self.subscribers:
            self.subscribers[topic] = []
        self.subscribers[topic].append(callback)
    
    async def publish(self, topic: str, update: StateUpdate):
        """
        Publish state update to all subscribers.
        This is the "restriction map" in sheaf theory.
        """
        if topic in self.subscribers:
            tasks = [
                asyncio.create_task(callback(update))
                for callback in self.subscribers[topic]
            ]
            await asyncio.gather(*tasks)
    
    async def publish_state_transition(self, transition: Transition):
        """
        Publish FSM transition to interested parties.
        Different subscribers see different "sections" of the state.
        """
        update = StateUpdate(
            old_state=transition.fromState,
            new_state=transition.toState,
            event=transition.input,
            timestamp=current_time(),
            node_id=self.node_id
        )
        
        # Publish to different views
        await self.publish("state.global", update)
        await self.publish(f"state.scope.{transition.scope_id}", update)
        await self.publish(f"state.node.{self.node_id}", update)
```

---

## Layer 2: Query Interface (CQRS Read Side)

### **Materialized Views from Event Stream**

```python
# read_model.py
from typing import Protocol

class ReadModel(Protocol):
    """
    A read-optimized projection of the FSM state.
    Built by replaying events from the event store.
    """
    def project(self, event: Event) -> None:
        """Update view based on event"""
        ...
    
    def query(self, query: Query) -> Result:
        """Execute query against materialized view"""
        ...

class ScopeTopologyView(ReadModel):
    """
    View optimized for querying the Zariski topology (lexical scope structure).
    This is Spec(R_Scheme) as a queryable data structure.
    """
    
    def __init__(self):
        self.scope_tree: Dict[ScopeId, ScopeNode] = {}
        self.binding_visibility: Dict[Identifier, Set[ScopeId]] = {}
    
    def project(self, event: Event):
        """Update topology based on event"""
        match event:
            case BindingCreated(id, scope, ts):
                # Update D(f) - the open set where f is visible
                if id not in self.binding_visibility:
                    self.binding_visibility[id] = set()
                self.binding_visibility[id].add(scope)
                
                # Update scope tree
                if scope not in self.scope_tree:
                    self.scope_tree[scope] = ScopeNode(scope)
            
            case ScopeEntered(scope_id, ts):
                # Extend scope chain
                current = self.get_current_scope()
                self.scope_tree[scope_id].parent = current
    
    def query(self, query: Query) -> Result:
        """
        Query the topological structure.
        Examples:
        - "Where is binding 'x' visible?" → return D(x)
        - "What's the Čech complex?" → return nerve of cover
        - "What's H¹?" → compute cohomology
        """
        match query:
            case WhereVisible(identifier):
                return self.binding_visibility.get(identifier, set())
            
            case ComputeCohomology():
                nerve = self.build_nerve()
                return compute_H1(nerve)
            
            case FindClosures(scope):
                return self.get_sheaf_sections(scope)

class ContinuationView(ReadModel):
    """
    View optimized for querying continuation structure.
    Maps FSM states to prime ideals.
    """
    
    def __init__(self):
        self.state_to_prime_ideal: Dict[State, PrimeIdeal] = {}
        self.continuation_history: List[Continuation] = []
    
    def project(self, event: Event):
        match event:
            case ContinuationCaptured(k, ts):
                # Compute corresponding prime ideal 𝔭_k
                prime_ideal = self.compute_prime_ideal(k)
                current_state = self.get_current_state()
                self.state_to_prime_ideal[current_state] = prime_ideal
                self.continuation_history.append(k)
    
    def query(self, query: Query) -> Result:
        match query:
            case GetPrimeIdeal(state):
                return self.state_to_prime_ideal.get(state)
            
            case GetContinuationSpace():
                return list(self.state_to_prime_ideal.values())

class ComplexityView(ReadModel):
    """
    View for computing H¹ and V(G) and verifying correspondence.
    """
    
    def __init__(self):
        self.cfg: ControlFlowGraph = ControlFlowGraph()
        self.scope_complex: CechComplex = CechComplex()
    
    def project(self, event: Event):
        # Update both CFG and scope complex
        self.cfg.add_event(event)
        self.scope_complex.add_event(event)
    
    def query(self, query: Query) -> Result:
        match query:
            case ComputeComplexityCorrespondence():
                H1 = compute_cohomology(self.scope_complex)
                VG = compute_cyclomatic(self.cfg)
                return CorrespondenceResult(
                    H1=H1,
                    VG=VG,
                    match=(H1 == VG or H1 == VG - 1)
                )
```

### **Query API**

```python
# query_service.py
class QueryService:
    """
    Provides read-only access to various materialized views.
    This is the "public key" - the observable interface.
    """
    
    def __init__(self):
        self.views = {
            'topology': ScopeTopologyView(),
            'continuations': ContinuationView(),
            'complexity': ComplexityView()
        }
        
        # Subscribe to state updates
        pubsub.subscribe("state.global", self.update_views)
    
    def update_views(self, update: StateUpdate):
        """Update all views when state changes"""
        for view in self.views.values():
            view.project(update.event)
    
    async def query(self, query: Query) -> Result:
        """Execute query against appropriate view"""
        view_name = query.target_view
        if view_name in self.views:
            return self.views[view_name].query(query)
        else:
            raise UnknownView(view_name)
```

---

## Layer 1: User Interface (Unidirectional Data Flow)

### **UDF Pattern for Predictable UI**

```typescript
// ui_layer.ts

// State = current FSM state (read-only to UI)
interface UIState {
  currentScope: ScopeId;
  visibleBindings: Set<Identifier>;
  activeContinuation: Continuation | null;
  cohomologyDimension: number;
  cyclomaticComplexity: number;
}

// Actions = user intentions (become commands)
type Action =
  | { type: 'CREATE_BINDING'; payload: { id: string; scope: string } }
  | { type: 'ENTER_SCOPE'; payload: { scopeId: string } }
  | { type: 'EXIT_SCOPE' }
  | { type: 'CAPTURE_CONTINUATION' }
  | { type: 'CALL_RPC'; payload: { node: string; method: string; args: any[] } };

// Dispatcher = validates and sends commands to Layer 4
class Dispatcher {
  constructor(private commandBus: CommandBus) {}
  
  async dispatch(action: Action): Promise<void> {
    // Transform action into command
    const command = this.actionToCommand(action);
    
    // Send to CQRS write side (Layer 4)
    try {
      const event = await this.commandBus.send(command);
      console.log('Command executed:', event);
    } catch (error) {
      console.error('Command failed:', error);
      // UI shows error
    }
  }
  
  private actionToCommand(action: Action): Command {
    switch (action.type) {
      case 'CREATE_BINDING':
        return new CreateBinding(action.payload.id, action.payload.scope);
      case 'ENTER_SCOPE':
        return new EnterScope(action.payload.scopeId);
      // ... etc
    }
  }
}

// View = renders current state
class View {
  constructor(
    private queryService: QueryService,
    private dispatcher: Dispatcher
  ) {}
  
  async render(): Promise<void> {
    // Query current state (from Layer 2)
    const topology = await this.queryService.query(
      new GetScopeTopology()
    );
    const complexity = await this.queryService.query(
      new ComputeComplexityCorrespondence()
    );
    
    // Render UI
    this.renderScopeTree(topology);
    this.renderComplexityMetrics(complexity);
  }
  
  // User interactions create actions
  onCreateBinding(id: string, scope: string): void {
    this.dispatcher.dispatch({
      type: 'CREATE_BINDING',
      payload: { id, scope }
    });
  }
}

// Unidirectional flow: Action → Dispatch → Command → Event → Query → View
```

### **MVC/MVVM Integration**

```typescript
// model_view_controller.ts

// Model = wrapper around query service (read-only)
class Model {
  constructor(private queryService: QueryService) {}
  
  async getCurrentState(): Promise<UIState> {
    const [topology, complexity, continuations] = await Promise.all([
      this.queryService.query(new GetScopeTopology()),
      this.queryService.query(new ComputeComplexityCorrespondence()),
      this.queryService.query(new GetContinuationSpace())
    ]);
    
    return {
      currentScope: topology.currentScope,
      visibleBindings: topology.visibleBindings,
      activeContinuation: continuations.latest,
      cohomologyDimension: complexity.H1,
      cyclomaticComplexity: complexity.VG
    };
  }
}

// Controller = handles user input
class Controller {
  constructor(
    private model: Model,
    private dispatcher: Dispatcher
  ) {}
  
  handleUserAction(action: Action): void {
    // Validate action at UI level (basic checks)
    if (this.isActionValid(action)) {
      // Dispatch to command layer
      this.dispatcher.dispatch(action);
    }
  }
  
  private isActionValid(action: Action): boolean {
    // Basic validation (type checking, etc.)
    // Full validation happens in Layer 4
    return true;
  }
}

// View = reactive rendering
class ReactiveView {
  constructor(
    private model: Model,
    private controller: Controller
  ) {
    // Subscribe to state updates
    pubsub.subscribe("state.global", () => this.refresh());
  }
  
  async refresh(): Promise<void> {
    const state = await this.model.getCurrentState();
    this.render(state);
  }
  
  render(state: UIState): void {
    // Render UI based on state
    console.log('Current State:', state);
  }
}
```

---

## Complete Data Flow

```
User Action (Layer 1)
  ↓
Dispatcher validates and converts to Command
  ↓
Command Bus sends to Layer 4 (FSM Core)
  ↓
FSM validates command → generates Event
  ↓
Event appended to Event Store
  ↓
FSM state transitions (deterministic)
  ↓
State update published via Pub/Sub (Layer 3)
  ↓
Subscribers replicate FSM state (consensus)
  ↓
Read Models project Event into views (Layer 2)
  ↓
Query Service provides updated state
  ↓
View refreshes UI (Layer 1)
  ↓
User sees result
```

---

## Summary: The Complete Pattern

| Layer | Pattern | Mathematical Role | Computational Role |
|-------|---------|-------------------|-------------------|
| 1. UI | UDF + MVC | Observable (WHY) | User/Agent Interface |
| 2. Query | CQRS Read + Views | Topological (HOW) | Materialized Projections |
| 3. Coordination | Pub/Sub + Replication | Distributed (WHERE) | State Synchronization |
| 4. Core | FSM + ES + CQRS Write | Algebraic (WHAT) | Deterministic Transitions |

This architecture is:
- **Mathematically sound**: FSM is discrete, deterministic, computable
- **Distributively correct**: State machine replication ensures consensus
- **Topologically consistent**: Read models are sheaf sections
- **Observable**: UI is structure-preserving functor

The FSM is the **discrete computational realization** of the continuous mathematical theory.