import type {
  ComputeH1Response,
  ComputeVGResponse,
  ValidateHypothesisResponse,
  ExecuteNLResponse,
} from '../services/types';
import './ResultsDisplay.css';

interface ResultsDisplayProps {
  h1Result?: ComputeH1Response | null;
  vgResult?: ComputeVGResponse | null;
  validationResult?: ValidateHypothesisResponse | null;
  nlResult?: ExecuteNLResponse | null;
  onClear?: () => void;
}

export default function ResultsDisplay({
  h1Result,
  vgResult,
  validationResult,
  nlResult,
  llmExplanation,
  onClear,
}: ResultsDisplayProps) {
  const hasResults = h1Result || vgResult || validationResult || nlResult;

  if (!hasResults) {
    return (
      <div className="results-container">
        <div className="results-placeholder">
          <p>Results will appear here after you run a computation.</p>
        </div>
      </div>
    );
  }

  return (
    <div className="results-container">
      <div className="results-header-actions">
        <h2>Results</h2>
        {onClear && (
          <button onClick={onClear} className="clear-button">
            Clear All
          </button>
        )}
      </div>

      {nlResult && (
        <div className="result-section">
          <h3>Natural Language Query Result</h3>
          {nlResult.success ? (
            <div className="result-content">
              {/* Show LLM explanation if available */}
              {llmExplanation && (
                <div className="llm-explanation">
                  <h4>LLM Explanation</h4>
                  <p>{llmExplanation}</p>
                </div>
              )}
              
              {/* Show LLM confidence if available */}
              {'llm_confidence' in nlResult && nlResult.llm_confidence !== undefined && (
                <div className="llm-confidence">
                  <strong>LLM Confidence:</strong> {(nlResult.llm_confidence * 100).toFixed(1)}%
                </div>
              )}
              
              {/* Show method used */}
              {'method' in nlResult && nlResult.method && (
                <div className="method-indicator">
                  <strong>Method:</strong> {nlResult.method}
                </div>
              )}
              
              <pre>{JSON.stringify(nlResult.result, null, 2)}</pre>
            </div>
          ) : (
            <div className="result-error">
              <strong>Error:</strong> {nlResult.error || 'Unknown error'}
            </div>
          )}
        </div>
      )}

      {h1Result && (
        <div className="result-section">
          <h3>H¹ Cohomology</h3>
          {h1Result.success ? (
            <div className="result-content">
              <div className="metric-grid">
                <div className="metric-item">
                  <span className="metric-label">H¹ Value:</span>
                  <span className="metric-value highlight">{h1Result.h1 ?? 'N/A'}</span>
                </div>
                <div className="metric-item">
                  <span className="metric-label">Bindings:</span>
                  <span className="metric-value">{h1Result.bindings ?? 'N/A'}</span>
                </div>
                <div className="metric-item">
                  <span className="metric-label">0-Simplices:</span>
                  <span className="metric-value">{h1Result.simplices_0 ?? 'N/A'}</span>
                </div>
                <div className="metric-item">
                  <span className="metric-label">1-Simplices:</span>
                  <span className="metric-value">{h1Result.simplices_1 ?? 'N/A'}</span>
                </div>
                <div className="metric-item">
                  <span className="metric-label">2-Simplices:</span>
                  <span className="metric-value">{h1Result.simplices_2 ?? 'N/A'}</span>
                </div>
              </div>
            </div>
          ) : (
            <div className="result-error">
              <strong>Error:</strong> {h1Result.error || 'Failed to compute H¹'}
            </div>
          )}
        </div>
      )}

      {vgResult && (
        <div className="result-section">
          <h3>V(G) Cyclomatic Complexity</h3>
          {vgResult.success ? (
            <div className="result-content">
              <div className="metric-grid">
                <div className="metric-item">
                  <span className="metric-label">V(G):</span>
                  <span className="metric-value highlight">{vgResult.v_g ?? 'N/A'}</span>
                </div>
              </div>
            </div>
          ) : (
            <div className="result-error">
              <strong>Error:</strong> {vgResult.error || 'Failed to compute V(G)'}
            </div>
          )}
        </div>
      )}

      {validationResult && (
        <div className="result-section">
          <h3>Hypothesis Validation</h3>
          <div className={`result-content ${validationResult.valid ? 'valid' : 'invalid'}`}>
            <div className="validation-status">
              <span className={`status-badge ${validationResult.valid ? 'valid' : 'invalid'}`}>
                {validationResult.valid ? '✓ Valid' : '✗ Invalid'}
              </span>
            </div>
            <div className="metric-grid">
              <div className="metric-item">
                <span className="metric-label">Difference:</span>
                <span className="metric-value">{validationResult.difference.toFixed(6)}</span>
              </div>
            </div>
            <div className="validation-message">
              {validationResult.message}
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
