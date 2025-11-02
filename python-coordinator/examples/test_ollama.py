#!/usr/bin/env python3
"""
Test Ollama LLM Integration

This script tests the Ollama LLM integration with the Computational Scheme Theory system.
"""

import sys
import os
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

import requests
from coordinator.llm_integration import EnhancedNLPCoordinator
from coordinator.llm_bridge import OllamaLLM, HAS_OLLAMA
import logging

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


def check_ollama_service():
    """Check if Ollama service is running"""
    try:
        response = requests.get("http://localhost:11434/api/tags", timeout=2)
        if response.status_code == 200:
            models = response.json().get("models", [])
            print(f"✓ Ollama service is running")
            print(f"  Available models: {[m.get('name', 'unknown') for m in models]}")
            return True
        else:
            print(f"✗ Ollama service returned status {response.status_code}")
            return False
    except requests.exceptions.ConnectionError:
        print("✗ Ollama service is not running")
        print("  Please start Ollama: ollama serve")
        return False
    except Exception as e:
        print(f"✗ Error checking Ollama service: {e}")
        return False


def test_ollama_direct():
    """Test Ollama LLM directly"""
    print("\n" + "="*70)
    print("Test 1: Direct Ollama LLM Interface")
    print("="*70)
    
    if not HAS_OLLAMA:
        print("✗ requests package not available (needed for Ollama)")
        print("  Install with: pip install requests")
        return False
    
    try:
        # Try to use an available model
        # Check what models are available
        try:
            models_response = requests.get("http://localhost:11434/api/tags", timeout=2)
            if models_response.status_code == 200:
                models = models_response.json().get("models", [])
                available_models = [m.get("name", "") for m in models]
                # Prefer smaller models for faster testing
                test_model = "gemma3:12b" if "gemma3:12b" in available_models else available_models[0] if available_models else "llama2"
                print(f"\nUsing model: {test_model}")
            else:
                test_model = "llama2"
        except:
            test_model = "llama2"
        
        llm = OllamaLLM(base_url="http://localhost:11434", model=test_model)
        
        # Test generation
        print("\n1. Testing text generation...")
        response = llm.generate("What is 2+2? Answer in one sentence.")
        print(f"   Response: {response}")
        
        # Test intent classification
        print("\n2. Testing intent classification...")
        result = llm.classify_intent("compute H1 for program test")
        print(f"   Operation: {result.get('operation', 'unknown')}")
        print(f"   Confidence: {result.get('confidence', 0.0):.2f}")
        print(f"   Reasoning: {result.get('reasoning', 'N/A')}")
        
        # Test embedding
        print("\n3. Testing embedding generation...")
        try:
            embedding = llm.embed("test query")
            print(f"   Embedding dimension: {len(embedding)}")
            print(f"   First few values: {embedding[:5]}")
        except Exception as e:
            print(f"   Note: Embedding failed (may not be supported by model): {e}")
        
        print("\n✓ Direct Ollama LLM test completed")
        return True
        
    except Exception as e:
        print(f"\n✗ Direct Ollama test failed: {e}")
        logger.exception("Error details:")
        return False


def test_ollama_coordinator():
    """Test Ollama through EnhancedNLPCoordinator"""
    print("\n" + "="*70)
    print("Test 2: Ollama via EnhancedNLPCoordinator")
    print("="*70)
    
    try:
        # Determine which model to use
        try:
            models_response = requests.get("http://localhost:11434/api/tags", timeout=2)
            if models_response.status_code == 200:
                models = models_response.json().get("models", [])
                available_models = [m.get("name", "") for m in models]
                test_model = "gemma3:12b" if "gemma3:12b" in available_models else available_models[0] if available_models else "llama2"
            else:
                test_model = "llama2"
        except:
            test_model = "llama2"
        
        print(f"\nInitializing coordinator with Ollama (model: {test_model})...")
        coordinator = EnhancedNLPCoordinator(
            llm_type="ollama",
            base_url="http://localhost:11434",
            model=test_model
        )
        
        if not coordinator.nlp_engine.llm:
            print("✗ LLM not initialized - check configuration")
            return False
        
        print("✓ Coordinator initialized")
        
        # Test intent classification
        print("\n1. Testing intent classification...")
        queries = [
            "compute H1 for program test",
            "calculate the cyclomatic complexity",
            "validate hypothesis for test program"
        ]
        
        for query in queries:
            print(f"\n   Query: {query}")
            result = coordinator.nlp_engine.process_query(query)
            print(f"   Operation: {result.get('operation', 'unknown')}")
            print(f"   Confidence: {result.get('confidence', 0.0):.2f}")
        
        # Test explanation generation
        print("\n2. Testing explanation generation...")
        operation = "computeH1"
        result = {
            "h1": 1,
            "num_bindings": 3,
            "num_simplices0": 5
        }
        explanation = coordinator.nlp_engine.generate_explanation(operation, result)
        print(f"   Explanation: {explanation}")
        
        # Test conversational interface
        print("\n3. Testing conversational interface...")
        response = coordinator.chat("What is H1 in computational scheme theory?")
        print(f"   Response: {response[:200]}...")  # Truncate for display
        
        print("\n✓ Coordinator test completed")
        return True
        
    except Exception as e:
        print(f"\n✗ Coordinator test failed: {e}")
        logger.exception("Error details:")
        return False


def test_ollama_full_pipeline():
    """Test full pipeline with Ollama"""
    print("\n" + "="*70)
    print("Test 3: Full Pipeline with Ollama")
    print("="*70)
    
    try:
        # Determine which model to use
        try:
            models_response = requests.get("http://localhost:11434/api/tags", timeout=2)
            if models_response.status_code == 200:
                models = models_response.json().get("models", [])
                available_models = [m.get("name", "") for m in models]
                test_model = "gemma3:12b" if "gemma3:12b" in available_models else available_models[0] if available_models else "llama2"
            else:
                test_model = "llama2"
        except:
            test_model = "llama2"
        
        coordinator = EnhancedNLPCoordinator(
            llm_type="ollama",
            base_url="http://localhost:11434",
            model=test_model
        )
        
        queries = [
            "compute H1 for program test",
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
                print(f"Error: {e}")
                logger.exception("Pipeline error:")
        
        print("\n✓ Full pipeline test completed")
        return True
        
    except Exception as e:
        print(f"\n✗ Full pipeline test failed: {e}")
        logger.exception("Error details:")
        return False


def main():
    """Run all Ollama tests"""
    print("\n" + "="*70)
    print("Ollama LLM Integration Test")
    print("="*70)
    
    print("\nPrerequisites:")
    print("1. Ollama service running: ollama serve")
    print("2. At least one model pulled: ollama pull llama2")
    print("3. Requests package installed: pip install requests")
    
    # Check prerequisites
    print("\nChecking prerequisites...")
    if not check_ollama_service():
        print("\n⚠ Please ensure Ollama is running before proceeding")
        print("  Start with: ollama serve")
        print("  Pull a model: ollama pull llama2")
        return
    
    if not HAS_OLLAMA:
        print("\n⚠ requests package not available")
        print("  Install with: pip install requests")
        return
    
    # Run tests
    results = []
    
    try:
        results.append(("Direct Ollama LLM", test_ollama_direct()))
        results.append(("Ollama via Coordinator", test_ollama_coordinator()))
        results.append(("Full Pipeline", test_ollama_full_pipeline()))
    except KeyboardInterrupt:
        print("\n\n⚠ Tests interrupted by user")
        return
    except Exception as e:
        logger.error(f"Test suite error: {e}", exc_info=True)
        return
    
    # Summary
    print("\n" + "="*70)
    print("Test Summary")
    print("="*70)
    for test_name, passed in results:
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"{status}: {test_name}")
    
    all_passed = all(result[1] for result in results)
    print("\n" + ("✓ All tests passed!" if all_passed else "✗ Some tests failed"))
    print("="*70 + "\n")


if __name__ == "__main__":
    main()

