"""
LLM Integration - Connects LLM bridge to Racket NLP system
"""

import subprocess
import json
from typing import Dict, Optional, List, Tuple
import logging
from pathlib import Path
import os

logger = logging.getLogger(__name__)

# Import LLM classes
try:
    from coordinator.llm_bridge import LLMInterface, OpenAILLM, LocalLLM, OllamaLLM
except ImportError:
    logger.warning("LLM bridge not available - LLM features disabled")
    LLMInterface = None
    OpenAILLM = None
    LocalLLM = None
    OllamaLLM = None


class HybridNLPEngine:
    """
    Hybrid NLP engine combining rule-based parsing (SGP-ASLN) with LLM capabilities
    """
    
    def __init__(self, llm: Optional[LLMInterface] = None):
        self.llm = llm
        self.conversation_history = []
    
    def process_query(self, query: str) -> Dict[str, any]:
        """
        Process query using hybrid approach:
        1. Try LLM intent classification if available
        2. Store in conversation history
        """
        if self.llm:
            try:
                llm_result = self.llm.classify_intent(query)
            except Exception as e:
                logger.warning(f"LLM classification failed: {e}, falling back to rule-based")
                llm_result = {
                    "operation": "unknown",
                    "confidence": 0.0,
                    "reasoning": f"LLM error: {str(e)}"
                }
        else:
            llm_result = {
                "operation": "unknown",
                "confidence": 0.0,
                "reasoning": "No LLM available"
            }
        
        # Store in conversation history
        self.conversation_history.append({
            "role": "user",
            "content": query
        })
        
        return llm_result
    
    def generate_explanation(self, operation: str, result: Dict) -> str:
        """Generate natural language explanation of results"""
        if not self.llm:
            return f"Operation {operation} completed. Result: {json.dumps(result, indent=2)}"
        
        prompt = f"""Explain this Computational Scheme Theory result:
Operation: {operation}
Result: {json.dumps(result, indent=2)}

Provide a clear, natural language explanation."""
        
        try:
            explanation = self.llm.generate(prompt, context=self.conversation_history)
            
            self.conversation_history.append({
                "role": "assistant",
                "content": explanation
            })
            
            return explanation
        except Exception as e:
            logger.error(f"Failed to generate explanation: {e}")
            return f"Operation {operation} completed. Result: {json.dumps(result, indent=2)}"
    
    def semantic_search(self, query: str, documents: List[str], top_k: int = 3) -> List[Tuple[str, float]]:
        """Semantic search using embeddings"""
        if not self.llm:
            # Fallback to simple string matching
            return [(doc, 0.5) for doc in documents[:top_k]]
        
        try:
            query_embedding = self.llm.embed(query)
            
            # Compute similarity with documents
            doc_embeddings = [self.llm.embed(doc) for doc in documents]
            
            # Cosine similarity
            import numpy as np
            query_vec = np.array(query_embedding)
            similarities = []
            for doc, doc_vec in zip(documents, doc_embeddings):
                doc_vec = np.array(doc_vec)
                norm_q = np.linalg.norm(query_vec)
                norm_d = np.linalg.norm(doc_vec)
                if norm_q > 0 and norm_d > 0:
                    similarity = np.dot(query_vec, doc_vec) / (norm_q * norm_d)
                else:
                    similarity = 0.0
                similarities.append((doc, float(similarity)))
            
            # Return top-k
            similarities.sort(key=lambda x: x[1], reverse=True)
            return similarities[:top_k]
        except Exception as e:
            logger.error(f"Semantic search failed: {e}")
            return [(doc, 0.5) for doc in documents[:top_k]]


class EnhancedNLPCoordinator:
    """
    Enhanced coordinator that uses LLM + existing Racket NLP
    """
    
    def __init__(self, llm_type: str = "openai", project_root: Optional[Path] = None, **llm_kwargs):
        """
        Initialize enhanced coordinator
        
        Args:
            llm_type: One of "openai", "local", "ollama", or "none"
            project_root: Path to project root (for Racket calls)
            **llm_kwargs: Additional arguments for LLM initialization
        """
        self.project_root = project_root or Path(__file__).parent.parent.parent
        self.llm_type = llm_type
        
        # Initialize LLM if requested
        if llm_type == "none" or LLMInterface is None:
            self.nlp_engine = HybridNLPEngine(llm=None)
        elif llm_type == "openai":
            if OpenAILLM is None:
                raise ImportError("OpenAI LLM not available. Install openai package.")
            api_key = llm_kwargs.get("api_key") or os.getenv("OPENAI_API_KEY")
            if not api_key:
                logger.warning("No OpenAI API key found. LLM features will be disabled.")
                self.nlp_engine = HybridNLPEngine(llm=None)
            else:
                llm = OpenAILLM(api_key=api_key, model=llm_kwargs.get("model", "gpt-4"))
                self.nlp_engine = HybridNLPEngine(llm=llm)
        elif llm_type == "local":
            if LocalLLM is None:
                raise ImportError("Local LLM not available. Install transformers and torch.")
            model_name = llm_kwargs.get("model_name", "microsoft/Phi-3-mini-4k-instruct")
            llm = LocalLLM(model_name=model_name)
            self.nlp_engine = HybridNLPEngine(llm=llm)
        elif llm_type == "ollama":
            if OllamaLLM is None:
                raise ImportError("Ollama LLM not available. Install requests package.")
            base_url = llm_kwargs.get("base_url", "http://localhost:11434")
            model = llm_kwargs.get("model", "llama2")
            llm = OllamaLLM(base_url=base_url, model=model)
            self.nlp_engine = HybridNLPEngine(llm=llm)
        else:
            raise ValueError(f"Unknown LLM type: {llm_type}")
    
    def process_nl_query(self, query: str) -> Dict:
        """
        Process natural language query:
        1. Use LLM for intent classification and understanding
        2. Call Racket SGP-ASLN for deterministic parsing if needed
        3. Execute computation
        4. Generate natural language response
        """
        # Step 1: LLM classification
        llm_result = self.nlp_engine.process_query(query)
        
        # Step 2: If operation identified with high confidence, use it
        # Otherwise fallback to Racket rule-based parsing
        confidence = llm_result.get("confidence", 0.0)
        operation = llm_result.get("operation", "unknown")
        
        if operation != "unknown" and confidence >= 0.7:
            # High confidence LLM result - use it
            logger.info(f"Using LLM result: {operation} (confidence: {confidence})")
            program_name = llm_result.get("program_name")
            params = llm_result.get("parameters", {})
            
            # Call Racket unified pipeline
            racket_result = self._call_racket_computation(
                operation,
                program_name,
                params
            )
            
            # Step 3: Generate explanation
            explanation = self.nlp_engine.generate_explanation(
                operation,
                {"llm_parse": llm_result, "racket_result": racket_result}
            )
            
            return {
                "operation": operation,
                "result": racket_result,
                "explanation": explanation,
                "llm_confidence": confidence,
                "method": "llm"
            }
        else:
            # Low confidence or unknown - try Racket fallback
            logger.info(f"LLM confidence low ({confidence}), falling back to Racket rule-based parsing")
            return self._fallback_to_racket(query)
    
    def _call_racket_computation(self, operation: str, program_name: Optional[str], params: Dict) -> Dict:
        """Call Racket unified pipeline via subprocess"""
        try:
            racket_script_dir = self.project_root / "racket-unified" / "src"
            racket_script = f"""
#lang racket/base
(require "nlp-integration.rkt")
(define query "{operation} {program_name or ''}")
(define result (process-nl-query-to-computation query))
(display (format "~a" result))
"""
            
            # Write temporary script
            import tempfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rkt', delete=False) as f:
                f.write(racket_script)
                temp_script = f.name
            
            try:
                result = subprocess.run(
                    ["racket", temp_script],
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=str(racket_script_dir.parent.parent)
                )
                
                if result.returncode == 0:
                    # Try to parse as JSON or return as string
                    try:
                        return json.loads(result.stdout)
                    except json.JSONDecodeError:
                        return {"raw_output": result.stdout.strip()}
                else:
                    logger.error(f"Racket computation failed: {result.stderr}")
                    return {"error": result.stderr}
            finally:
                # Clean up temp file
                try:
                    os.unlink(temp_script)
                except:
                    pass
                    
        except Exception as e:
            logger.error(f"Error calling Racket computation: {e}")
            return {"error": str(e)}
    
    def _fallback_to_racket(self, query: str) -> Dict:
        """Fallback to pure Racket rule-based parsing"""
        try:
            racket_script_dir = self.project_root / "racket-unified" / "src"
            racket_script = f"""
#lang racket/base
(require "nlp-integration.rkt")
(define result (process-nl-query-to-computation "{query}"))
(display (format "~a" result))
"""
            
            import tempfile
            with tempfile.NamedTemporaryFile(mode='w', suffix='.rkt', delete=False) as f:
                f.write(racket_script)
                temp_script = f.name
            
            try:
                result = subprocess.run(
                    ["racket", temp_script],
                    capture_output=True,
                    text=True,
                    timeout=30,
                    cwd=str(racket_script_dir.parent.parent)
                )
                
                if result.returncode == 0:
                    try:
                        return {
                            "fallback": True,
                            "result": json.loads(result.stdout) if result.stdout.strip() else result.stdout,
                            "method": "racket_rule_based"
                        }
                    except json.JSONDecodeError:
                        return {
                            "fallback": True,
                            "result": result.stdout.strip(),
                            "method": "racket_rule_based"
                        }
                else:
                    return {
                        "fallback": True,
                        "error": result.stderr,
                        "method": "racket_rule_based"
                    }
            finally:
                try:
                    os.unlink(temp_script)
                except:
                    pass
        except Exception as e:
            logger.error(f"Racket fallback failed: {e}")
            return {"error": str(e), "method": "fallback_failed"}
    
    def chat(self, message: str) -> str:
        """Interactive chat interface"""
        if self.nlp_engine.llm:
            try:
                response = self.nlp_engine.llm.generate(
                    message,
                    context=self.nlp_engine.conversation_history
                )
                self.nlp_engine.conversation_history.append({
                    "role": "assistant",
                    "content": response
                })
                return response
            except Exception as e:
                logger.error(f"Chat error: {e}")
                return f"I'm sorry, I encountered an error: {str(e)}"
        else:
            return "LLM features are not available. Please configure an LLM provider."

