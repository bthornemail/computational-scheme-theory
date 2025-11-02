import { useState } from 'react';
import { apiService } from '../services/api';
import type { ComputeH1Response, ComputeVGResponse } from '../services/types';
import './SchemeEditor.css';

interface SchemeEditorProps {
  onH1Result?: (result: ComputeH1Response) => void;
  onVGResult?: (result: ComputeVGResponse) => void;
}

const defaultCode = `(lambda (x)
  (lambda (y)
    (+ x y)))`;

export default function SchemeEditor({ onH1Result, onVGResult }: SchemeEditorProps) {
  const [code, setCode] = useState(defaultCode);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleComputeH1 = async () => {
    setLoading(true);
    setError(null);

    try {
      const result = await apiService.computeH1({ source_code: code });
      onH1Result?.(result);
      if (!result.success) {
        setError(result.error || 'Failed to compute H¹');
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Unknown error occurred';
      setError(message);
      onH1Result?.({ success: false, error: message });
    } finally {
      setLoading(false);
    }
  };

  const handleComputeVG = async () => {
    setLoading(true);
    setError(null);

    try {
      const result = await apiService.computeVG({ source_code: code });
      onVGResult?.(result);
      if (!result.success) {
        setError(result.error || 'Failed to compute V(G)');
      }
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Unknown error occurred';
      setError(message);
      onVGResult?.({ success: false, error: message });
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setCode('');
    setError(null);
  };

  return (
    <div className="scheme-editor-container">
      <div className="editor-header">
        <h2>Scheme Code Editor</h2>
        <p className="subtitle">Write or paste R5RS Scheme code</p>
      </div>

      <div className="editor-wrapper">
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          className="code-editor"
          placeholder="Enter Scheme code here..."
          spellCheck={false}
          disabled={loading}
        />
      </div>

      {error && (
        <div className="error-message">
          <strong>Error:</strong> {error}
        </div>
      )}

      <div className="editor-actions">
        <button
          onClick={handleComputeH1}
          disabled={loading || !code.trim()}
          className="action-button primary"
        >
          {loading ? 'Computing...' : 'Compute H¹'}
        </button>
        <button
          onClick={handleComputeVG}
          disabled={loading || !code.trim()}
          className="action-button primary"
        >
          {loading ? 'Computing...' : 'Compute V(G)'}
        </button>
        <button
          onClick={handleClear}
          disabled={loading}
          className="action-button secondary"
        >
          Clear
        </button>
      </div>
    </div>
  );
}
