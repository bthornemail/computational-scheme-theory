import { useState } from 'react';
import NaturalLanguageQuery from './components/NaturalLanguageQuery';
import SchemeEditor from './components/SchemeEditor';
import ResultsDisplay from './components/ResultsDisplay';
import LLMSettings from './components/LLMSettings';
import { apiService } from './services/api';
import type {
  ComputeH1Response,
  ComputeVGResponse,
  ValidateHypothesisResponse,
  ExecuteNLResponse,
  LLMProcessNLResponse,
} from './services/types';
import './App.css';

type Tab = 'nl' | 'editor';

function App() {
  const [activeTab, setActiveTab] = useState<Tab>('nl');
  const [h1Result, setH1Result] = useState<ComputeH1Response | null>(null);
  const [vgResult, setVGResult] = useState<ComputeVGResponse | null>(null);
  const [validationResult, setValidationResult] = useState<ValidateHypothesisResponse | null>(null);
  const [nlResult, setNLResult] = useState<ExecuteNLResponse | null>(null);

  const handleNLResult = async (result: ExecuteNLResponse) => {
    setNLResult(result);
    
    // Try to extract H1/VG results from NL execution
    if (result.success && result.result) {
      // Check if result contains H1 data
      if (result.result.h1 !== undefined) {
        setH1Result({
          success: true,
          h1: result.result.h1,
          bindings: result.result.bindings,
          simplices_0: result.result.simplices_0,
          simplices_1: result.result.simplices_1,
          simplices_2: result.result.simplices_2,
        });
      }
      
      // Check if result contains VG data
      if (result.result.v_g !== undefined) {
        setVGResult({
          success: true,
          v_g: result.result.v_g,
        });
      }
      
      // Check if result contains validation data
      if (result.result.valid !== undefined) {
        setValidationResult({
          valid: result.result.valid,
          difference: result.result.difference || 0,
          message: result.result.message || 'Validation completed',
        });
      }
    }
  };

  const handleH1Result = async (result: ComputeH1Response) => {
    setH1Result(result);
    
    // If we have both H1 and VG, validate hypothesis
    if (result.success && result.h1 !== undefined && vgResult?.success && vgResult.v_g !== undefined) {
      try {
        const validation = await apiService.validateHypothesis({
          h1: result.h1,
          v_g: vgResult.v_g,
          k: 0,
          tolerance: 0.001,
        });
        setValidationResult(validation);
      } catch (err) {
        console.error('Failed to validate hypothesis:', err);
      }
    }
  };

  const handleVGResult = async (result: ComputeVGResponse) => {
    setVGResult(result);
    
    // If we have both H1 and VG, validate hypothesis
    if (result.success && result.v_g !== undefined && h1Result?.success && h1Result.h1 !== undefined) {
      try {
        const validation = await apiService.validateHypothesis({
          h1: h1Result.h1,
          v_g: result.v_g,
          k: 0,
          tolerance: 0.001,
        });
        setValidationResult(validation);
      } catch (err) {
        console.error('Failed to validate hypothesis:', err);
      }
    }
  };

  const handleClearResults = () => {
    setH1Result(null);
    setVGResult(null);
    setValidationResult(null);
    setNLResult(null);
    setLLMExplanation(null);
  };

  return (
    <div className="app">
      <header className="app-header">
        <div className="header-content">
          <h1>Computational Scheme Theory</h1>
          <p className="header-subtitle">
            Empirical Validation of H?(X_Comp, O_Comp) = V(G) - k
          </p>
        </div>
      </header>

      <main className="app-main">
        <div className="tabs">
          <button
            className={`tab-button ${activeTab === 'nl' ? 'active' : ''}`}
            onClick={() => setActiveTab('nl')}
          >
            Natural Language Query
          </button>
          <button
            className={`tab-button ${activeTab === 'editor' ? 'active' : ''}`}
            onClick={() => setActiveTab('editor')}
          >
            Scheme Code Editor
          </button>
        </div>

        <div className="content-area">
          <div className="input-panel">
            {activeTab === 'nl' ? (
              <>
                <LLMSettings onLLMTypeChange={setLLMType} />
                <NaturalLanguageQuery 
                  onResult={handleNLResult}
                  llmType={llmType}
                  useLLM={llmType !== 'none'}
                />
              </>
            ) : (
              <SchemeEditor onH1Result={handleH1Result} onVGResult={handleVGResult} />
            )}
          </div>

          <div className="results-panel">
            <ResultsDisplay
              h1Result={h1Result}
              vgResult={vgResult}
              validationResult={validationResult}
              nlResult={nlResult}
              llmExplanation={llmExplanation}
              onClear={handleClearResults}
            />
          </div>
        </div>
      </main>

      <footer className="app-footer">
        <p>
          Computational Scheme Theory - Unified Lisp Substrate
          {' | '}
          <a href="https://github.com" target="_blank" rel="noopener noreferrer">
            Documentation
          </a>
        </p>
      </footer>
    </div>
  );
}

export default App;
