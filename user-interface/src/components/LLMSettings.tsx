import { useState, useEffect } from 'react';
import { apiService, type LLMStatusResponse } from '../services/api';
import './LLMSettings.css';

interface LLMSettingsProps {
  onLLMTypeChange?: (llmType: string) => void;
}

export default function LLMSettings({ onLLMTypeChange }: LLMSettingsProps) {
  const [llmType, setLLMType] = useState<string>('none');
  const [status, setStatus] = useState<LLMStatusResponse | null>(null);
  const [checking, setChecking] = useState(false);

  useEffect(() => {
    // Check LLM status on mount
    checkLLMStatus();
  }, [llmType]);

  const checkLLMStatus = async () => {
    setChecking(true);
    try {
      const result = await apiService.getLLMStatus(llmType);
      setStatus(result);
    } catch (err) {
      setStatus({
        available: false,
        error: err instanceof Error ? err.message : 'Failed to check LLM status',
      });
    } finally {
      setChecking(false);
    }
  };

  const handleLLMTypeChange = (newType: string) => {
    setLLMType(newType);
    onLLMTypeChange?.(newType);
  };

  return (
    <div className="llm-settings-container">
      <div className="llm-settings-header">
        <h3>LLM Enhancement</h3>
        <button
          onClick={checkLLMStatus}
          disabled={checking}
          className="refresh-button"
          title="Check LLM availability"
        >
          {checking ? 'Checking...' : '↻'}
        </button>
      </div>

      <div className="llm-type-selector">
        <label>
          <input
            type="radio"
            name="llm-type"
            value="none"
            checked={llmType === 'none'}
            onChange={(e) => handleLLMTypeChange(e.target.value)}
          />
          <span>None (Rule-based only)</span>
        </label>
        <label>
          <input
            type="radio"
            name="llm-type"
            value="openai"
            checked={llmType === 'openai'}
            onChange={(e) => handleLLMTypeChange(e.target.value)}
          />
          <span>OpenAI</span>
        </label>
        <label>
          <input
            type="radio"
            name="llm-type"
            value="ollama"
            checked={llmType === 'ollama'}
            onChange={(e) => handleLLMTypeChange(e.target.value)}
          />
          <span>Ollama (Local)</span>
        </label>
        <label>
          <input
            type="radio"
            name="llm-type"
            value="local"
            checked={llmType === 'local'}
            onChange={(e) => handleLLMTypeChange(e.target.value)}
          />
          <span>Local Transformers</span>
        </label>
      </div>

      {status && (
        <div className={`llm-status ${status.available ? 'available' : 'unavailable'}`}>
          {status.available ? (
            <span className="status-indicator success">✓ LLM Available</span>
          ) : (
            <span className="status-indicator error">
              ✗ LLM Not Available
              {status.error && <span className="error-detail">{status.error}</span>}
            </span>
          )}
        </div>
      )}

      {llmType !== 'none' && !status?.available && (
        <div className="llm-setup-info">
          <p className="info-text">
            <strong>Setup Required:</strong>
          </p>
          {llmType === 'openai' && (
            <ul>
              <li>Set <code>OPENAI_API_KEY</code> environment variable</li>
              <li>Install: <code>pip install openai</code></li>
            </ul>
          )}
          {llmType === 'ollama' && (
            <ul>
              <li>Install and run Ollama: <code>ollama serve</code></li>
              <li>Default URL: <code>http://localhost:11434</code></li>
            </ul>
          )}
          {llmType === 'local' && (
            <ul>
              <li>Install: <code>pip install transformers torch</code></li>
              <li>May require significant disk space for models</li>
            </ul>
          )}
        </div>
      )}
    </div>
  );
}
