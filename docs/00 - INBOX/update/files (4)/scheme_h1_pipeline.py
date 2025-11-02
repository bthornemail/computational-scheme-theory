"""
Complete Pipeline: Scheme → Datalog → H¹ Computation
Includes Y-combinator detection and projective type support

This bridges:
- S-expressions (Scheme concrete syntax)
- M-expressions (abstract syntax / AST)
- Datalog (logic programming representation)
- Incidence structure (geometric representation)
- H¹ cohomology (topological invariant)
"""

from dataclasses import dataclass
from typing import List, Dict, Union, Optional, Set, Tuple
from enum import Enum
import re

# ============================================================================
# PART 1: M-EXPRESSION AST (Abstract Syntax)
# ============================================================================

class TypeClass(Enum):
    AFFINE = "affine"
    PROJECTIVE = "projective"

@dataclass
class MExpr:
    """Base class for M-expressions (abstract syntax)"""
    pass

@dataclass
class Var(MExpr):
    name: str

@dataclass
class Literal(MExpr):
    value: Union[int, bool, str]

@dataclass
class Lambda(MExpr):
    params: List[str]
    body: 'MExpr'

@dataclass
class Apply(MExpr):
    func: MExpr
    args: List[MExpr]

@dataclass
class If(MExpr):
    test: MExpr
    then_branch: MExpr
    else_branch: MExpr

@dataclass
class Let(MExpr):
    bindings: List[Tuple[str, MExpr]]  # [(name, value), ...]
    body: MExpr

@dataclass
class Define(MExpr):
    name: str
    value: MExpr

@dataclass
class Undefined(MExpr):
    """Represents projective point at infinity"""
    pass

# ============================================================================
# PART 2: S-EXPRESSION PARSER
# ============================================================================

def tokenize(text: str) -> List[str]:
    """Tokenize Scheme S-expression"""
    # Simple tokenizer
    text = text.replace('(', ' ( ').replace(')', ' ) ')
    return text.split()

def parse_sexp(tokens: List[str]) -> Union[list, str]:
    """Parse tokens into nested list structure"""
    if not tokens:
        raise SyntaxError("Unexpected EOF")
    
    token = tokens.pop(0)
    
    if token == '(':
        sexp = []
        while tokens[0] != ')':
            sexp.append(parse_sexp(tokens))
        tokens.pop(0)  # Remove ')'
        return sexp
    elif token == ')':
        raise SyntaxError("Unexpected )")
    else:
        # Try to parse as number
        try:
            return int(token)
        except ValueError:
            return token

def sexp_to_mexp(sexp: Union[list, str]) -> MExpr:
    """Convert S-expression to M-expression (AST)"""
    
    # Atom
    if isinstance(sexp, (int, str)) and not isinstance(sexp, list):
        if isinstance(sexp, int):
            return Literal(sexp)
        elif sexp in ['#t', '#f']:
            return Literal(sexp == '#t')
        else:
            return Var(sexp)
    
    # List
    if not sexp:
        return Literal(None)
    
    head = sexp[0]
    
    # Special forms
    if head == 'lambda':
        # (lambda (params...) body)
        params = sexp[1]
        body = sexp_to_mexp(sexp[2])
        return Lambda(params, body)
    
    elif head == 'if':
        # (if test then else)
        test = sexp_to_mexp(sexp[1])
        then_branch = sexp_to_mexp(sexp[2])
        else_branch = sexp_to_mexp(sexp[3]) if len(sexp) > 3 else Undefined()
        return If(test, then_branch, else_branch)
    
    elif head == 'let':
        # (let ((var val) ...) body)
        bindings = [(name, sexp_to_mexp(val)) for name, val in sexp[1]]
        body = sexp_to_mexp(sexp[2])
        return Let(bindings, body)
    
    elif head == 'define':
        # (define name value) or (define (name params...) body)
        if isinstance(sexp[1], list):
            # Function definition
            name = sexp[1][0]
            params = sexp[1][1:]
            body = sexp_to_mexp(sexp[2])
            return Define(name, Lambda(params, body))
        else:
            # Variable definition
            name = sexp[1]
            value = sexp_to_mexp(sexp[2])
            return Define(name, value)
    
    elif head == 'quote' or head == "'":
        # Quoted symbol - treat as undefined for now
        return Undefined()
    
    else:
        # Function application
        func = sexp_to_mexp(head)
        args = [sexp_to_mexp(arg) for arg in sexp[1:]]
        return Apply(func, args)

def parse_scheme(text: str) -> MExpr:
    """Parse Scheme text to M-expression AST"""
    tokens = tokenize(text)
    sexp = parse_sexp(tokens)
    return sexp_to_mexp(sexp)

# ============================================================================
# PART 3: DATALOG FACT GENERATION
# ============================================================================

class DatalogGenerator:
    """Generate Datalog facts from M-expression AST"""
    
    def __init__(self):
        self.scope_counter = 0
        self.facts: List[str] = []
        self.bindings: Set[Tuple[str, str, str]] = set()  # (name, scope, type)
        self.constraints: Set[Tuple[str, str]] = set()  # (constraint, scope)
        self.incidences: Set[Tuple[str, str]] = set()  # (binding, constraint)
        self.dependencies: Set[Tuple[str, str]] = set()  # (from, to)
        self.access_map: Dict[str, int] = {}  # binding-name -> access count
    
    def fresh_scope(self) -> str:
        """Generate fresh scope identifier"""
        scope = f"scope_{self.scope_counter}"
        self.scope_counter += 1
        return scope
    
    def add_binding(self, name: str, scope: str, type_class: str = "affine"):
        """Add a binding fact"""
        self.bindings.add((name, scope, type_class))
        self.facts.append(f"binding({name}, {scope}, {type_class}).")
    
    def add_constraint(self, constraint: str, scope: str):
        """Add a constraint fact"""
        self.constraints.add((constraint, scope))
        self.facts.append(f"constraint({constraint}, {scope}).")
    
    def add_incidence(self, binding: str, constraint: str):
        """Add an incidence relation"""
        self.incidences.add((binding, constraint))
        self.facts.append(f"incidence({binding}, {constraint}).")
    
    def add_dependency(self, from_var: str, to_var: str):
        """Add a dependency edge"""
        self.dependencies.add((from_var, to_var))
        self.facts.append(f"depends({from_var}, {to_var}).")
    
    def is_y_combinator(self, expr: MExpr) -> bool:
        """Detect Y-combinator pattern"""
        if not isinstance(expr, Lambda):
            return False
        
        # Y = λf. (λx. f (x x)) (λx. f (x x))
        # Simplified check: lambda with self-application
        if isinstance(expr.body, Apply):
            if isinstance(expr.body.func, Lambda):
                # Check for (x x) pattern
                body = expr.body.func.body
                if isinstance(body, Apply) and isinstance(body.func, Apply):
                    return True
        
        return False
    
    def count_accesses(self, expr: MExpr) -> None:
        """Count how many times each binding is accessed (Church numeral computation)
        Each variable reference increments access count, which becomes dimension
        """
        if isinstance(expr, Var):
            # Variable reference - increment access count
            name = expr.name
            self.access_map[name] = self.access_map.get(name, 0) + 1
        
        elif isinstance(expr, Lambda):
            # Recurse on body
            self.count_accesses(expr.body)
        
        elif isinstance(expr, Apply):
            # Recurse on function and all arguments
            self.count_accesses(expr.func)
            for arg in expr.args:
                self.count_accesses(arg)
        
        elif isinstance(expr, If):
            # Recurse on all branches
            self.count_accesses(expr.test)
            self.count_accesses(expr.then_branch)
            self.count_accesses(expr.else_branch)
        
        elif isinstance(expr, Let):
            # Recurse on binding values and body
            for _, value in expr.bindings:
                self.count_accesses(value)
            self.count_accesses(expr.body)
        
        elif isinstance(expr, Define):
            # Recurse on value
            self.count_accesses(expr.value)
        
        # Literal and Undefined don't have accesses
    
    def generate(self, expr: MExpr, scope: str = None) -> None:
        """Generate Datalog facts from M-expression"""
        
        # Count accesses first (Church numeral computation)
        self.count_accesses(expr)
        
        if scope is None:
            scope = self.fresh_scope()
        
        if isinstance(expr, Var):
            # Variable reference
            self.add_binding(expr.name, scope, "affine")
        
        elif isinstance(expr, Literal):
            # Literal value - no binding
            pass
        
        elif isinstance(expr, Lambda):
            # Lambda creates new scope
            lambda_scope = self.fresh_scope()
            
            # Check for Y-combinator
            if self.is_y_combinator(expr):
                self.add_constraint("y_combinator", lambda_scope)
                self.add_constraint("fixed_point", lambda_scope)
                self.facts.append(f"creates_cycle({lambda_scope}).")
            
            # Parameters are bindings
            for param in expr.params:
                self.add_binding(param, lambda_scope, "affine")
            
            # Recurse on body
            self.generate(expr.body, lambda_scope)
        
        elif isinstance(expr, Apply):
            # Function application
            app_scope = self.fresh_scope()
            
            self.generate(expr.func, app_scope)
            for arg in expr.args:
                self.generate(arg, app_scope)
            
            # Add application constraint
            self.add_constraint("application", app_scope)
        
        elif isinstance(expr, If):
            # Conditional - creates branching
            if_scope = self.fresh_scope()
            
            self.generate(expr.test, if_scope)
            
            # Check for projective branch (undefined)
            if isinstance(expr.else_branch, Undefined):
                # Projective point at infinity!
                undefined_name = f"undefined_{if_scope}"
                self.add_binding(undefined_name, if_scope, "projective")
                self.add_constraint("projective_closure", if_scope)
                self.add_incidence(undefined_name, "projective_closure")
            else:
                self.generate(expr.else_branch, if_scope)
            
            self.generate(expr.then_branch, if_scope)
            
            # Add branch convergence constraint
            self.add_constraint("branch_convergence", if_scope)
        
        elif isinstance(expr, Let):
            # Let creates bindings
            let_scope = self.fresh_scope()
            
            for name, value in expr.bindings:
                self.add_binding(name, let_scope, "affine")
                self.generate(value, let_scope)
                
                # Add dependency constraint
                constraint_name = f"{name}_deps"
                self.add_constraint(constraint_name, let_scope)
                self.add_incidence(name, constraint_name)
            
            self.generate(expr.body, let_scope)
        
        elif isinstance(expr, Define):
            # Define creates global binding
            define_scope = self.fresh_scope()
            
            self.add_binding(expr.name, define_scope, "affine")
            self.generate(expr.value, define_scope)
            
            # Check if value is Y-combinator application
            if isinstance(expr.value, Apply):
                if isinstance(expr.value.func, Var) and expr.value.func.name == 'Y':
                    self.add_constraint("uses_y_combinator", define_scope)
                    self.add_incidence(expr.name, "uses_y_combinator")
        
        elif isinstance(expr, Undefined):
            # Projective point at infinity
            undef_name = f"undefined_{scope}"
            self.add_binding(undef_name, scope, "projective")

# ============================================================================
# PART 4: DATALOG QUERY ENGINE (SIMPLE)
# ============================================================================

class SimpleDatalog:
    """Simple Datalog query engine (limited functionality)"""
    
    def __init__(self):
        self.facts: Set[Tuple[str, ...]] = set()
        self.rules: List[Tuple[str, List[str]]] = []
    
    def assert_fact(self, fact_str: str):
        """Add a fact from string"""
        # Parse fact: predicate(arg1, arg2, ...).
        match = re.match(r'(\w+)\((.*)\)\.', fact_str)
        if match:
            predicate = match.group(1)
            args = [arg.strip() for arg in match.group(2).split(',')]
            self.facts.add((predicate, *args))
    
    def query(self, query_str: str) -> List[Dict[str, str]]:
        """Execute a query"""
        # Simple query: predicate(X, Y) where X, Y are variables
        match = re.match(r'(\w+)\((.*)\)', query_str)
        if not match:
            return []
        
        predicate = match.group(1)
        args = [arg.strip() for arg in match.group(2).split(',')]
        
        results = []
        for fact in self.facts:
            if fact[0] == predicate:
                # Try to match args
                bindings = {}
                match = True
                for i, arg in enumerate(args, 1):
                    if arg[0].isupper():  # Variable
                        bindings[arg] = fact[i]
                    elif arg != fact[i]:  # Constant mismatch
                        match = False
                        break
                
                if match:
                    results.append(bindings)
        
        return results
    
    def get_all_facts(self, predicate: str) -> List[Tuple]:
        """Get all facts for a predicate"""
        return [fact[1:] for fact in self.facts if fact[0] == predicate]

# ============================================================================
# PART 5: INTEGRATION WITH H¹ COMPUTATION
# ============================================================================

from h1_incidence_computation import Point, Hyperplane, IncidenceStructure

def build_incidence_from_datalog(generator: DatalogGenerator) -> IncidenceStructure:
    """Build incidence structure from Datalog facts
    Uses access_map to populate dimension and access_count fields
    """
    
    # Extract points (bindings) with dimensional tracking
    points = []
    point_map = {}
    
    for name, scope, type_class in generator.bindings:
        is_proj = (type_class == "projective")
        # Get access count from access_map (Church numeral)
        access_count = generator.access_map.get(name, 0)
        # Dimension = access count = Church numeral = polynomial degree
        dimension = access_count
        point = Point(f"{name}@{scope}", is_proj, dimension, access_count)
        points.append(point)
        point_map[f"{name}@{scope}"] = len(points) - 1
    
    # Extract hyperplanes (constraints)
    hyperplanes = []
    hyper_map = {}
    
    for constraint, scope in generator.constraints:
        is_proj_closure = ("projective_closure" in constraint or 
                          "fixed_point" in constraint)
        hyper = Hyperplane(f"{constraint}@{scope}", is_proj_closure)
        hyperplanes.append(hyper)
        hyper_map[f"{constraint}@{scope}"] = len(hyperplanes) - 1
    
    # Build structure
    structure = IncidenceStructure(points, hyperplanes)
    
    # Add incidences from facts
    for binding, constraint in generator.incidences:
        # Find matching point and hyperplane
        for (b_name, b_scope, _) in generator.bindings:
            if b_name == binding:
                point_key = f"{b_name}@{b_scope}"
                if point_key in point_map:
                    # Find matching constraint
                    for (c_name, c_scope) in generator.constraints:
                        if c_name == constraint:
                            hyper_key = f"{c_name}@{c_scope}"
                            if hyper_key in hyper_map:
                                structure.add_incidence(
                                    point_map[point_key],
                                    hyper_map[hyper_key]
                                )
                                break
                    break
    
    # Also add implicit incidences (binding in same scope as constraint)
    for (b_name, b_scope, _) in generator.bindings:
        for (c_name, c_scope) in generator.constraints:
            if b_scope == c_scope:
                point_key = f"{b_name}@{b_scope}"
                hyper_key = f"{c_name}@{c_scope}"
                if point_key in point_map and hyper_key in hyper_map:
                    structure.add_incidence(
                        point_map[point_key],
                        hyper_map[hyper_key]
                    )
    
    return structure

def compute_h1_from_scheme(scheme_text: str, verbose: bool = True) -> int:
    """Complete pipeline: Scheme → H¹"""
    
    if verbose:
        print("\n" + "="*70)
        print("SCHEME TO H¹ PIPELINE")
        print("="*70)
        print(f"\nInput Scheme program:")
        print("-" * 70)
        print(scheme_text)
        print("-" * 70)
    
    # Step 1: Parse to M-expression
    if verbose:
        print("\n[Step 1] Parsing S-expression to M-expression AST...")
    
    m_expr = parse_scheme(scheme_text)
    
    if verbose:
        print(f"  AST root: {type(m_expr).__name__}")
    
    # Step 2: Generate Datalog facts
    if verbose:
        print("\n[Step 2] Generating Datalog facts...")
    
    generator = DatalogGenerator()
    generator.generate(m_expr)
    
    if verbose:
        print(f"  Bindings: {len(generator.bindings)}")
        print(f"  Constraints: {len(generator.constraints)}")
        print(f"  Incidences: {len(generator.incidences)}")
        
        print("\n  Key Datalog facts:")
        for fact in generator.facts[:20]:  # Show first 20
            print(f"    {fact}")
        if len(generator.facts) > 20:
            print(f"    ... ({len(generator.facts) - 20} more)")
    
    # Step 3: Build incidence structure
    if verbose:
        print("\n[Step 3] Building incidence structure...")
    
    structure = build_incidence_from_datalog(generator)
    
    if verbose:
        structure.print_structure()
    
    # Step 4: Compute H¹
    if verbose:
        print("\n[Step 4] Computing H¹...")
    
    h1 = structure.compute_H1(verbose=verbose)
    
    return h1

# ============================================================================
# PART 6: EXAMPLES
# ============================================================================

if __name__ == "__main__":
    print("\n" + "█"*70)
    print("█" + " "*68 + "█")
    print("█" + "  SCHEME → DATALOG → H¹ COMPUTATION".center(68) + "█")
    print("█" + "  With Y-Combinator and Projective Type Support".center(68) + "█")
    print("█" + " "*68 + "█")
    print("█"*70)
    
    # Example 1: Simple factorial (direct recursion)
    example1 = """
    (define (factorial n)
      (if (<= n 1)
          1
          (* n (factorial (- n 1)))))
    """
    
    print("\n" + "="*70)
    print("EXAMPLE 1: Direct Recursion (Factorial)")
    print("="*70)
    h1_1 = compute_h1_from_scheme(example1, verbose=True)
    
    # Example 2: Y-combinator factorial
    example2 = """
    (define Y
      (lambda (f)
        ((lambda (x) (f (lambda (v) ((x x) v))))
         (lambda (x) (f (lambda (v) ((x x) v)))))))
    """
    
    print("\n" + "="*70)
    print("EXAMPLE 2: Y-Combinator Definition")
    print("="*70)
    h1_2 = compute_h1_from_scheme(example2, verbose=True)
    
    # Example 3: Safe divide with projective point
    example3 = """
    (define (safe-divide x y)
      (if (= y 0)
          'undefined
          (/ x y)))
    """
    
    print("\n" + "="*70)
    print("EXAMPLE 3: Safe Divide (Projective Point)")
    print("="*70)
    h1_3 = compute_h1_from_scheme(example3, verbose=True)
    
    # Summary
    print("\n" + "█"*70)
    print("█" + " SUMMARY ".center(68) + "█")
    print("█"*70)
    print(f"█  Example 1 (Direct recursion):    H¹ = {h1_1}".ljust(69) + "█")
    print(f"█  Example 2 (Y-combinator):        H¹ = {h1_2}".ljust(69) + "█")
    print(f"█  Example 3 (Projective point):    H¹ = {h1_3}".ljust(69) + "█")
    print("█"*70)
    
    print("\nKey Observations:")
    print("- Y-combinators create fixed points → should increase H¹")
    print("- Projective points (undefined) add cycles through infinity")
    print("- This pipeline bridges S-expr → M-expr → Datalog → Geometry → H¹")
