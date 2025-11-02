"""
LLM Bridge - Integrates language models with the Computational Scheme Theory system
"""

from typing import List, Dict, Optional, Tuple
import json
from abc import ABC, abstractmethod
import logging

logger = logging.getLogger(__name__)

# Option A: Use OpenAI API
try:
    import openai
    HAS_OPENAI = True
except ImportError:
    HAS_OPENAI = False
    logger.debug("OpenAI package not available")

# Option B: Use local models via transformers
try:
    from transformers import AutoTokenizer, AutoModelForCausalLM, pipeline
    import torch
    HAS_TRANSFORMERS = True
except ImportError:
    HAS_TRANSFORMERS = False
    logger.debug("Transformers package not available")

# Option C: Use Ollama for local LLMs
try:
    import requests
    HAS_OLLAMA = True
except ImportError:
    HAS_OLLAMA = False
    logger.debug("requests package not available for Ollama")


class LLMInterface(ABC):
    """Abstract interface for LLM providers"""
    
    @abstractmethod
    def generate(self, prompt: str, context: Optional[List[Dict]] = None) -> str:
        """Generate text from prompt"""
        pass
    
    @abstractmethod
    def embed(self, text: str) -> List[float]:
        """Generate embedding vector for text"""
        pass
    
    @abstractmethod
    def classify_intent(self, query: str) -> Dict[str, any]:
        """Classify user intent using LLM"""
        pass


class OpenAILLM(LLMInterface):
    """OpenAI GPT integration"""
    
    def __init__(self, api_key: Optional[str] = None, model: str = "gpt-4"):
        if not HAS_OPENAI:
            raise ImportError("openai package required. Install with: pip install openai")
        self.client = openai.OpenAI(api_key=api_key)
        self.model = model
        self.embedding_model = "text-embedding-ada-002"
    
    def generate(self, prompt: str, context: Optional[List[Dict]] = None) -> str:
        messages = []
        if context:
            messages.extend(context)
        messages.append({"role": "user", "content": prompt})
        
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=messages,
                temperature=0.7,
                max_tokens=1000
            )
            return response.choices[0].message.content
        except Exception as e:
            logger.error(f"OpenAI generation error: {e}")
            raise
    
    def embed(self, text: str) -> List[float]:
        try:
            response = self.client.embeddings.create(
                model=self.embedding_model,
                input=text
            )
            return response.data[0].embedding
        except Exception as e:
            logger.error(f"OpenAI embedding error: {e}")
            raise
    
    def classify_intent(self, query: str) -> Dict[str, any]:
        """Use LLM to classify intent and extract parameters"""
        system_prompt = """You are an NLP assistant for Computational Scheme Theory.
Parse queries and return JSON with:
- operation: one of [computeH1, computeVG, validateHypothesis, analyzePatterns, exportPolynomial, getPatternDimensions, explain, generate]
- program_name: extracted program identifier if mentioned
- parameters: any parameters like k=value
- confidence: 0.0-1.0 confidence score
- reasoning: brief explanation

Return only valid JSON, no markdown code blocks."""
        
        prompt = f"Parse this query: {query}"
        try:
            response = self.generate(f"{system_prompt}\n\n{prompt}")
            
            # Try to extract JSON from response
            json_str = response.strip()
            
            # Remove markdown code blocks if present
            if "```json" in json_str:
                json_str = json_str.split("```json")[1].split("```")[0].strip()
            elif "```" in json_str:
                json_str = json_str.split("```")[1].split("```")[0].strip()
            
            result = json.loads(json_str)
            
            # Ensure required fields
            if "operation" not in result:
                result["operation"] = "unknown"
            if "confidence" not in result:
                result["confidence"] = 0.5
            
            return result
        except json.JSONDecodeError as e:
            logger.warning(f"Failed to parse LLM JSON response: {e}")
            return {
                "operation": "unknown",
                "reasoning": response if 'response' in locals() else str(e),
                "confidence": 0.3
            }
        except Exception as e:
            logger.error(f"LLM intent classification error: {e}")
            return {
                "operation": "unknown",
                "reasoning": str(e),
                "confidence": 0.0
            }


class LocalLLM(LLMInterface):
    """Local LLM using transformers (e.g., Llama, Mistral, Phi-3)"""
    
    def __init__(self, model_name: str = "microsoft/Phi-3-mini-4k-instruct"):
        if not HAS_TRANSFORMERS:
            raise ImportError("transformers package required. Install with: pip install transformers torch")
        
        logger.info(f"Loading local model: {model_name}")
        self.tokenizer = AutoTokenizer.from_pretrained(model_name)
        self.model = AutoModelForCausalLM.from_pretrained(
            model_name,
            torch_dtype=torch.float16 if torch.cuda.is_available() else torch.float32,
            device_map="auto"
        )
        self.pipeline = pipeline(
            "text-generation",
            model=self.model,
            tokenizer=self.tokenizer
        )
        logger.info("Local model loaded successfully")
    
    def generate(self, prompt: str, context: Optional[List[Dict]] = None) -> str:
        if context:
            # Build context string
            context_str = "\n".join([f"{m['role']}: {m['content']}" for m in context])
            full_prompt = f"{context_str}\nuser: {prompt}\nassistant:"
        else:
            full_prompt = f"user: {prompt}\nassistant:"
        
        try:
            outputs = self.pipeline(
                full_prompt,
                max_new_tokens=500,
                temperature=0.7,
                do_sample=True
            )
            generated = outputs[0]['generated_text']
            # Extract assistant response
            if "assistant:" in generated:
                return generated.split("assistant:")[-1].strip()
            return generated.replace(full_prompt, "").strip()
        except Exception as e:
            logger.error(f"Local LLM generation error: {e}")
            raise
    
    def embed(self, text: str) -> List[float]:
        """Use model's embedding layer or a separate embedding model"""
        try:
            inputs = self.tokenizer(text, return_tensors="pt", truncation=True, max_length=512)
            with torch.no_grad():
                outputs = self.model(**inputs, output_hidden_states=True)
                # Use mean pooling of last hidden state
                embeddings = outputs.hidden_states[-1].mean(dim=1).squeeze().tolist()
            return embeddings
        except Exception as e:
            logger.error(f"Local LLM embedding error: {e}")
            raise
    
    def classify_intent(self, query: str) -> Dict[str, any]:
        prompt = f"""Parse this Computational Scheme Theory query and return JSON:
Query: {query}
Response (JSON only with operation, program_name, parameters, confidence, reasoning):"""
        
        try:
            response = self.generate(prompt)
            # Try to extract JSON
            json_str = response.strip()
            if "```json" in json_str:
                json_str = json_str.split("```json")[1].split("```")[0].strip()
            elif "```" in json_str:
                json_str = json_str.split("```")[1].split("```")[0].strip()
            
            result = json.loads(json_str)
            
            # Ensure required fields
            if "operation" not in result:
                result["operation"] = "unknown"
            if "confidence" not in result:
                result["confidence"] = 0.5
            
            return result
        except (json.JSONDecodeError, Exception) as e:
            logger.warning(f"Failed to parse local LLM JSON response: {e}")
            return {
                "operation": "unknown",
                "reasoning": response if 'response' in locals() else str(e),
                "confidence": 0.3
            }


class OllamaLLM(LLMInterface):
    """Ollama local LLM integration"""
    
    def __init__(self, base_url: str = "http://localhost:11434", model: str = "llama2"):
        if not HAS_OLLAMA:
            raise ImportError("requests package required. Install with: pip install requests")
        self.base_url = base_url
        self.model = model
    
    def generate(self, prompt: str, context: Optional[List[Dict]] = None) -> str:
        # Build prompt with context if provided
        full_prompt = prompt
        if context:
            context_str = "\n".join([f"{m['role']}: {m['content']}" for m in context])
            full_prompt = f"{context_str}\nuser: {prompt}\nassistant:"
        
        try:
            # Try /api/chat first (newer API) - but skip if we know it's not available
            chat_available = None
            try:
                messages = []
                if context:
                    messages.extend(context)
                messages.append({"role": "user", "content": prompt})
                
                response = requests.post(
                    f"{self.base_url}/api/chat",
                    json={"model": self.model, "messages": messages},
                    timeout=30
                )
                
                if response.status_code == 200:
                    # Chat API typically returns streaming JSON (one object per line)
                    # Parse line-by-line to extract the complete message
                    content = response.text.strip()
                    message_content = ""
                    
                    for line in content.split('\n'):
                        if line.strip():
                            try:
                                line_json = json.loads(line)
                                # Check for message content in this line
                                if "message" in line_json:
                                    msg = line_json["message"]
                                    if isinstance(msg, dict) and "content" in msg:
                                        message_content += msg["content"]
                                    elif isinstance(msg, str):
                                        message_content += msg
                                # Also check for direct response field
                                elif "response" in line_json:
                                    message_content += line_json["response"]
                            except json.JSONDecodeError:
                                continue
                    
                    if message_content:
                        return message_content.strip()
                    
                    # Fallback: try parsing as single JSON object
                    try:
                        result = response.json()
                        if "message" in result:
                            msg = result["message"]
                            if isinstance(msg, dict) and "content" in msg:
                                return msg["content"]
                            elif isinstance(msg, str):
                                return msg
                        if "response" in result:
                            return result["response"]
                    except (json.JSONDecodeError, ValueError):
                        pass
                else:
                    # Chat API not available (404) or other error
                    chat_available = False
            except requests.exceptions.HTTPError as e:
                # Chat API failed, will use generate API
                chat_available = False
                logger.debug(f"Chat API failed ({e}), using generate API")
            except (KeyError, json.JSONDecodeError) as e:
                logger.debug(f"Chat API parse error ({e}), trying generate API")
            
            # Use /api/generate (more compatible API)
            response = requests.post(
                f"{self.base_url}/api/generate",
                json={"model": self.model, "prompt": full_prompt, "stream": False},
                timeout=30
            )
            response.raise_for_status()
            
            # Parse response - Ollama returns single JSON object with "response" field
            content = response.text.strip()
            
            # Try parsing as single JSON object first
            try:
                result = json.loads(content)
                if "response" in result:
                    return result["response"].strip()
                elif "message" in result and "content" in result["message"]:
                    return result["message"]["content"].strip()
            except json.JSONDecodeError:
                # Handle streaming JSON format (one JSON object per line)
                response_text = ""
                for line in content.split('\n'):
                    if line.strip():
                        try:
                            line_json = json.loads(line)
                            if "response" in line_json:
                                response_text += line_json["response"]
                        except json.JSONDecodeError:
                            continue
                
                if response_text:
                    return response_text.strip()
            
            # If we get here, we couldn't extract a response
            raise ValueError(f"Could not extract response from Ollama. Status: {response.status_code}, Content: {content[:500]}")
            
        except Exception as e:
            logger.error(f"Ollama generation error: {e}")
            raise
    
    def embed(self, text: str) -> List[float]:
        try:
            response = requests.post(
                f"{self.base_url}/api/embeddings",
                json={"model": self.model, "prompt": text},
                timeout=30
            )
            response.raise_for_status()
            return response.json()["embedding"]
        except Exception as e:
            logger.error(f"Ollama embedding error: {e}")
            raise
    
    def classify_intent(self, query: str) -> Dict[str, any]:
        prompt = f"""Parse this Computational Scheme Theory query and return JSON:
Query: {query}
Response (JSON only with operation, program_name, parameters, confidence, reasoning):"""
        
        try:
            response = self.generate(prompt)
            # Try to extract JSON
            json_str = response.strip()
            if "```json" in json_str:
                json_str = json_str.split("```json")[1].split("```")[0].strip()
            elif "```" in json_str:
                json_str = json_str.split("```")[1].split("```")[0].strip()
            
            result = json.loads(json_str)
            
            # Ensure required fields
            if "operation" not in result:
                result["operation"] = "unknown"
            if "confidence" not in result:
                result["confidence"] = 0.5
            
            return result
        except (json.JSONDecodeError, Exception) as e:
            logger.warning(f"Failed to parse Ollama JSON response: {e}")
            return {
                "operation": "unknown",
                "reasoning": response if 'response' in locals() else str(e),
                "confidence": 0.3
            }

