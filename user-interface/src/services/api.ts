/**
 * API Service for Computational Scheme Theory
 * 
 * Communicates with the backend API server that bridges to the Racket MCP server
 */

import type {
  ComputeH1Request,
  ComputeH1Response,
  ComputeVGRequest,
  ComputeVGResponse,
  ValidateHypothesisRequest,
  ValidateHypothesisResponse,
  ProcessNLRequest,
  ProcessNLResponse,
  ExecuteNLRequest,
  ExecuteNLResponse,
  LLMStatusResponse,
  LLMProcessNLRequest,
  LLMProcessNLResponse,
  LLMExplainRequest,
  LLMExplainResponse,
} from './types';

// Re-export types for convenience
export type {
  ComputeH1Request,
  ComputeH1Response,
  ComputeVGRequest,
  ComputeVGResponse,
  ValidateHypothesisRequest,
  ValidateHypothesisResponse,
  ProcessNLRequest,
  ProcessNLResponse,
  ExecuteNLRequest,
  ExecuteNLResponse,
  LLMStatusResponse,
  LLMProcessNLRequest,
  LLMProcessNLResponse,
  LLMExplainRequest,
  LLMExplainResponse,
} from './types';

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:3001/api';

class ApiService {
  private baseUrl: string;

  constructor(baseUrl: string = API_BASE_URL) {
    this.baseUrl = baseUrl;
  }

  private async request<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<T> {
    const url = `${this.baseUrl}${endpoint}`;
    const response = await fetch(url, {
      ...options,
      headers: {
        'Content-Type': 'application/json',
        ...options.headers,
      },
    });

    if (!response.ok) {
      const error = await response.json().catch(() => ({ error: 'Unknown error' }));
      throw new Error(error.error || `HTTP ${response.status}: ${response.statusText}`);
    }

    return response.json();
  }

  async computeH1(request: ComputeH1Request): Promise<ComputeH1Response> {
    return this.request<ComputeH1Response>('/compute-h1', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async computeVG(request: ComputeVGRequest): Promise<ComputeVGResponse> {
    return this.request<ComputeVGResponse>('/compute-vg', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async validateHypothesis(
    request: ValidateHypothesisRequest
  ): Promise<ValidateHypothesisResponse> {
    return this.request<ValidateHypothesisResponse>('/validate-hypothesis', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async processNaturalLanguage(
    request: ProcessNLRequest
  ): Promise<ProcessNLResponse> {
    return this.request<ProcessNLResponse>('/process-nl', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async executeNaturalLanguage(
    request: ExecuteNLRequest
  ): Promise<ExecuteNLResponse> {
    return this.request<ExecuteNLResponse>('/execute-nl', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async getLLMStatus(llmType: string = 'openai'): Promise<LLMStatusResponse> {
    return this.request<LLMStatusResponse>(`/llm/status?llm_type=${llmType}`, {
      method: 'GET',
    });
  }

  async processNaturalLanguageWithLLM(
    request: LLMProcessNLRequest
  ): Promise<LLMProcessNLResponse> {
    return this.request<LLMProcessNLResponse>('/llm/process-nl', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }

  async generateLLMExplanation(
    request: LLMExplainRequest
  ): Promise<LLMExplainResponse> {
    return this.request<LLMExplainResponse>('/llm/explain', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  }
}

export const apiService = new ApiService();
export default apiService;
