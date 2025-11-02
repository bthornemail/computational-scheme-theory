import { useState } from 'react';
import { apiService } from '../services/api';
import type { ExecuteNLResponse, LLMProcessNLResponse } from '../services/types';
import './NaturalLanguageQuery.css';

interface NaturalLanguageQueryProps {
  onResult: (result: ExecuteNLResponse | LLMProcessNLResponse) => void;
  llmType?: string;
  useLLM?: boolean;
}

export default function NaturalLanguageQuery({ onResult, llmType = 'none', useLLM = false }: NaturalLanguageQueryProps) {
  const [query, setQuery] = useState('');
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const exampleQueries = [
    'compute H1 for program test',
    'compute V(G) for program test',
    'validate hypothesis for program test',
    'export polynomial for program test',
    'get pattern dimensions for program test',
  ];

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!query.trim()) return;

    setLoading(true);
    setError(null);

    try {
      if (useLLM && llmType !== 'none') {
        // Use LLM-enhanced processing
        const result = await apiService.processNaturalLanguageWithLLM({
          query,
          llm_type: llmType as 'openai' | 'local' | 'ollama',
        });
        
        // Convert LLM response to ExecuteNLResponse format
        if (result.error) {
          onResult({ success: false, error: result.error });
        } else {
          onResult({
            success: true,
            result: {
              ...result,
              llm_enhanced: true,
              confidence: result.llm_confidence,
            },
          });
        }
      } else {
        // Use rule-based processing
        const result = await apiService.executeNaturalLanguage({ query });
        onResult(result);
      }
      
      setQuery('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Unknown error occurred');
      onResult({ success: false, error: err instanceof Error ? err.message : 'Unknown error' });
    } finally {
      setLoading(false);
    }
  };

  const handleExampleClick = (example: string) => {
    setQuery(example);
  };

  return (
    <div className="nl-query-container">
      <div className="nl-query-header">
        <h2>Natural Language Query</h2>
        <p className="subtitle">
          Ask questions in plain English
          {useLLM && llmType !== 'none' && (
            <span className="llm-badge">LLM Enhanced</span>
          )}
        </p>
      </div>

      <form onSubmit={handleSubmit} className="nl-query-form">
        <div className="input-group">
          <textarea
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            placeholder="e.g., compute H1 for program test"
            className="query-input"
            rows={3}
            disabled={loading}
          />
          <button
            type="submit"
            disabled={loading || !query.trim()}
            className="submit-button"
          >
            {loading ? 'Processing...' : 'Execute Query'}
          </button>
        </div>

        {error && (
          <div className="error-message">
            <strong>Error:</strong> {error}
          </div>
        )}
      </form>

      <div className="examples-section">
        <h3>Example Queries</h3>
        <div className="examples-list">
          {exampleQueries.map((example, idx) => (
            <button
              key={idx}
              onClick={() => handleExampleClick(example)}
              className="example-button"
              disabled={loading}
            >
              {example}
            </button>
          ))}
        </div>
      </div>
    </div>
  );
}
