/**
 * API Types for Computational Scheme Theory
 */

export interface ComputeH1Request {
  source_code: string;
}

export interface ComputeH1Response {
  success: boolean;
  h1?: number;
  bindings?: number;
  simplices_0?: number;
  simplices_1?: number;
  simplices_2?: number;
  error?: string;
}

export interface ComputeVGRequest {
  source_code: string;
}

export interface ComputeVGResponse {
  success: boolean;
  v_g?: number;
  error?: string;
}

export interface ValidateHypothesisRequest {
  h1: number;
  v_g: number;
  k?: number;
  tolerance?: number;
}

export interface ValidateHypothesisResponse {
  valid: boolean;
  difference: number;
  message: string;
}

export interface ProcessNLRequest {
  query: string;
}

export interface ProcessNLResponse {
  success: boolean;
  m_expression?: {
    op: string;
    args: string[];
  };
  raw_query?: string;
  error?: string;
}

export interface ExecuteNLRequest {
  query: string;
}

export interface ExecuteNLResponse {
  success: boolean;
  result?: any;
  error?: string;
}

// LLM Types

export interface LLMStatusResponse {
  available: boolean;
  llm_type?: string;
  error?: string;
}

export interface LLMProcessNLRequest {
  query: string;
  llm_type?: 'openai' | 'local' | 'ollama' | 'none';
}

export interface LLMProcessNLResponse {
  operation?: string;
  result?: any;
  explanation?: string;
  llm_confidence?: number;
  method?: string;
  error?: string;
  success?: boolean;
}

export interface LLMExplainRequest {
  operation: string;
  result: any;
  llm_type?: 'openai' | 'local' | 'ollama' | 'none';
}

export interface LLMExplainResponse {
  explanation?: string;
  success: boolean;
  error?: string;
}
