#!/usr/bin/env python3
"""
LLM Integration Demo - Demonstrates LLM-enhanced NLP capabilities

This script shows how to use the LLM integration with the Computational Scheme Theory system.
"""

import os
import sys
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from coordinator.llm_integration import EnhancedNLPCoordinator
import logging

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


def demo_intent_classification():
    """Demonstrate LLM intent classification"""
    print("\n" + "="*70)
    print("Demo 1: Intent Classification")
    print("="*70)
    
    # Initialize coordinator (will use OpenAI if API key set, otherwise rule-based)
    llm_type = os.getenv("LLM_TYPE", "openai")  # or "local", "ollama", "none"
    
    try:
        coordinator = EnhancedNLPCoordinator(llm_type=llm_type)
    except Exception as e:
        logger.warning(f"Could not initialize LLM coordinator: {e}")
        logger.info("Falling back to rule-based parsing")
        coordinator = EnhancedNLPCoordinator(llm_type="none")
    
    queries = [
        "compute H1 for program test",
        "calculate the first cohomology group",
        "what is the cyclomatic complexity of my factorial function?",
        "validate hypothesis H1 equals V(G) minus k",
        "export polynomial representation",
        "get pattern dimensions for program test"
    ]
    
    for query in queries:
        print(f"\nQuery: {query}")
        result = coordinator.nlp_engine.process_query(query)
        
        if coordinator.nlp_engine.llm:
            print(f"  Operation: {result.get('operation', 'unknown')}")
            print(f"  Confidence: {result.get('confidence', 0.0):.2f}")
            print(f"  Reasoning: {result.get('reasoning', 'N/A')}")
        else:
            print("  (Rule-based parsing - LLM not available)")


def demo_natural_language_explanations():
    """Demonstrate natural language explanation generation"""
    print("\n" + "="*70)
    print("Demo 2: Natural Language Explanations")
    print("="*70)
    
    llm_type = os.getenv("LLM_TYPE", "openai")
    
    try:
        coordinator = EnhancedNLPCoordinator(llm_type=llm_type)
    except Exception as e:
        logger.warning(f"Could not initialize LLM coordinator: {e}")
        print("  (LLM not available - cannot generate explanations)")
        return
    
    if not coordinator.nlp_engine.llm:
        print("  (LLM not available - cannot generate explanations)")
        return
    
    # Example computation result
    operation = "computeH1"
    result = {
        "h1": 1,
        "num_bindings": 3,
        "num_simplices0": 5,
        "num_simplices1": 4,
        "num_simplices2": 1
    }
    
    print(f"\nOperation: {operation}")
    print(f"Result: {result}")
    print("\nGenerated Explanation:")
    explanation = coordinator.nlp_engine.generate_explanation(operation, result)
    print(f"  {explanation}")


def demo_conversational_queries():
    """Demonstrate conversational interface"""
    print("\n" + "="*70)
    print("Demo 3: Conversational Interface")
    print("="*70)
    
    llm_type = os.getenv("LLM_TYPE", "openai")
    
    try:
        coordinator = EnhancedNLPCoordinator(llm_type=llm_type)
    except Exception as e:
        logger.warning(f"Could not initialize LLM coordinator: {e}")
        print("  (LLM not available - conversational interface disabled)")
        return
    
    if not coordinator.nlp_engine.llm:
        print("  (LLM not available - conversational interface disabled)")
        return
    
    messages = [
        "What is Computational Scheme Theory?",
        "How do I compute H1?",
        "Can you explain the relationship between H1 and V(G)?"
    ]
    
    for message in messages:
        print(f"\nUser: {message}")
        response = coordinator.chat(message)
        print(f"Assistant: {response}")


def demo_semantic_search():
    """Demonstrate semantic search using embeddings"""
    print("\n" + "="*70)
    print("Demo 4: Semantic Search")
    print("="*70)
    
    llm_type = os.getenv("LLM_TYPE", "openai")
    
    try:
        coordinator = EnhancedNLPCoordinator(llm_type=llm_type)
    except Exception as e:
        logger.warning(f"Could not initialize LLM coordinator: {e}")
        print("  (LLM not available - semantic search disabled)")
        return
    
    if not coordinator.nlp_engine.llm:
        print("  (LLM not available - semantic search disabled)")
        return
    
    documents = [
        "Compute the first cohomology group H1",
        "Calculate cyclomatic complexity V(G)",
        "Validate the hypothesis H1 = V(G) - k",
        "Export polynomial representation",
        "Get pattern dimensions"
    ]
    
    query = "find cohomology"
    print(f"\nQuery: {query}")
    print("\nSearch Results:")
    
    results = coordinator.nlp_engine.semantic_search(query, documents, top_k=3)
    for i, (doc, score) in enumerate(results, 1):
        print(f"  {i}. [{score:.3f}] {doc}")


def demo_full_pipeline():
    """Demonstrate full pipeline: NL query → LLM → Racket → Explanation"""
    print("\n" + "="*70)
    print("Demo 5: Full Pipeline Integration")
    print("="*70)
    
    llm_type = os.getenv("LLM_TYPE", "openai")
    
    try:
        coordinator = EnhancedNLPCoordinator(llm_type=llm_type)
    except Exception as e:
        logger.warning(f"Could not initialize LLM coordinator: {e}")
        coordinator = EnhancedNLPCoordinator(llm_type="none")
    
    queries = [
        "compute H1 for program test",
        "calculate V(G) for test",
    ]
    
    for query in queries:
        print(f"\nQuery: {query}")
        print("-" * 70)
        
        try:
            result = coordinator.process_nl_query(query)
            
            print(f"Operation: {result.get('operation', 'unknown')}")
            print(f"Method: {result.get('method', 'unknown')}")
            
            if 'explanation' in result:
                print(f"\nExplanation:\n  {result['explanation']}")
            
            if 'result' in result:
                print(f"\nResult: {result['result']}")
        except Exception as e:
            logger.error(f"Error processing query: {e}")
            print(f"Error: {e}")


def main():
    """Run all demos"""
    print("\n" + "="*70)
    print("LLM Integration Demo for Computational Scheme Theory")
    print("="*70)
    
    print("\nNote: Set OPENAI_API_KEY environment variable to use OpenAI")
    print("      Set LLM_TYPE environment variable to choose provider:")
    print("        - 'openai' (default)")
    print("        - 'local' (requires transformers)")
    print("        - 'ollama' (requires Ollama service)")
    print("        - 'none' (rule-based only)")
    
    try:
        demo_intent_classification()
        demo_natural_language_explanations()
        demo_conversational_queries()
        demo_semantic_search()
        demo_full_pipeline()
    except KeyboardInterrupt:
        print("\n\nDemo interrupted by user")
    except Exception as e:
        logger.error(f"Demo error: {e}", exc_info=True)
    
    print("\n" + "="*70)
    print("Demo Complete")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()

