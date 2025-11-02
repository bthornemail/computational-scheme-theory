"""
Direct Computation Module

This module provides direct computation of H¹ and V(G) without requiring
services to be running. It uses subprocess calls to execute Racket programs
from the unified implementation (racket-unified/src/algorithms/).

Both H¹ and V(G) are computed using the unified Racket pipeline.
"""

import subprocess
import tempfile
import os
from pathlib import Path
from typing import Tuple, Optional
import logging

logger = logging.getLogger(__name__)


def compute_h1_direct(source_code: str, program_id: str, project_root: Path) -> Tuple[int, int, int, int, float]:
    """
    Compute H¹-H⁴ directly by calling Racket unified pipeline
    
    Args:
        source_code: Scheme source code
        program_id: Program identifier
        project_root: Root of the project
        
    Returns:
        Tuple of (h1_value, h2_value, h3_value, h4_value, computation_time_ms)
    """
    import time
    start_time = time.time()
    
    # Write source to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.scm', delete=False) as f:
        f.write(source_code)
        temp_file = f.name
    
    try:
        # Use the unified Racket implementation
        racket_unified_dir = project_root / "racket-unified" / "src"
        
        # Create a Racket script file that uses the unified pipeline
        script_file = racket_unified_dir / "compute_h1_temp.rkt"
        try:
            with open(script_file, 'w') as f:
                f.write(f"""#lang racket/base
(require racket/file
         "algorithms/unified-pipeline.rkt")

(define source (file->string "{temp_file}"))
(with-handlers ([exn? (lambda (e) (displayln "0 0 0 0"))])
  (define result (compute-h1-from-source-detailed source))
  (if (pipeline-result-success result)
      (displayln (string-append
                   (number->string (pipeline-result-h1 result)) " "
                   (number->string (pipeline-result-h2-value result)) " "
                   (number->string (pipeline-result-h3-value result)) " "
                   (number->string (pipeline-result-h4-value result))))
      (displayln "0 0 0 0")))
""")
            
            # Run Racket script from racket_unified_dir using absolute path
            result = subprocess.run(
                ["racket", str(script_file.resolve())],
                cwd=str(racket_unified_dir.resolve()),
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                try:
                    parts = result.stdout.strip().split()
                    h1 = int(parts[0]) if len(parts) > 0 else 0
                    h2 = int(parts[1]) if len(parts) > 1 else 0
                    h3 = int(parts[2]) if len(parts) > 2 else 0
                    h4 = int(parts[3]) if len(parts) > 3 else 0
                    elapsed_ms = (time.time() - start_time) * 1000
                    logger.info(f"Computed H¹={h1}, H²={h2}, H³={h3}, H⁴={h4} for {program_id}")
                    return h1, h2, h3, h4, elapsed_ms
                except (ValueError, IndexError):
                    logger.warning(f"Could not parse cohomology values from output: {result.stdout}")
            
            logger.warning(f"Racket H¹-H⁴ computation failed: {result.stderr}")
            return 0, 0, 0, 0, (time.time() - start_time) * 1000
            
        finally:
            # Clean up script file
            try:
                if script_file.exists():
                    os.unlink(script_file)
            except:
                pass
        
    except FileNotFoundError:
        logger.warning("racket not found, using placeholder")
        return 0, 0, 0, 0, (time.time() - start_time) * 1000
    except subprocess.TimeoutExpired:
        logger.error(f"Racket H¹-H⁴ computation timed out for {program_id}")
        return 0, 0, 0, 0, (time.time() - start_time) * 1000
    except Exception as e:
        logger.error(f"Error computing H¹-H⁴: {e}")
        return 0, 0, 0, 0, (time.time() - start_time) * 1000
    finally:
        # Clean up temp file
        try:
            os.unlink(temp_file)
        except:
            pass


def compute_vg_direct(source_code: str, program_id: str, project_root: Path) -> Tuple[int, float]:
    """
    Compute V(G) directly by calling Racket program
    
    Args:
        source_code: Scheme source code
        program_id: Program identifier
        project_root: Root of the project
        
    Returns:
        Tuple of (vg_value, computation_time_ms)
    """
    import time
    start_time = time.time()
    
    # Write source to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.scm', delete=False) as f:
        f.write(source_code)
        temp_file = f.name
    
    try:
        # Use the new unified implementation
        racket_unified_dir = project_root / "racket-unified" / "src"
        
        # Create a Racket script file that uses the unified pipeline
        script_file = racket_unified_dir / "compute_vg_temp.rkt"
        try:
            with open(script_file, 'w') as f:
                f.write(f"""#lang racket/base
(require racket/file
         "algorithms/cfg-builder.rkt"
         "algorithms/cyclomatic.rkt")

(define source (file->string "{temp_file}"))
(with-handlers ([exn? (lambda (e) (displayln "0"))])
  (define cfg (build-cfg-from-source source))
  (define metrics (compute-cyclomatic-complexity cfg))
  (displayln (complexity-metrics-v-g metrics)))
""")
            
            # Run Racket script from racket_unified_dir using absolute path
            result = subprocess.run(
                ["racket", str(script_file.resolve())],
                cwd=str(racket_unified_dir.resolve()),
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                try:
                    vg = int(result.stdout.strip())
                    elapsed_ms = (time.time() - start_time) * 1000
                    logger.info(f"Computed V(G)={vg} for {program_id}")
                    return vg, elapsed_ms
                except ValueError:
                    logger.warning(f"Could not parse V(G) from output: {result.stdout}")
            
            logger.warning(f"Racket computation failed: {result.stderr}")
            return 0, (time.time() - start_time) * 1000
            
        finally:
            # Clean up script file
            try:
                if script_file.exists():
                    os.unlink(script_file)
            except:
                pass
        
    except FileNotFoundError:
        logger.warning("racket not found, using placeholder")
        return 0, (time.time() - start_time) * 1000
    except subprocess.TimeoutExpired:
        logger.error(f"Racket computation timed out for {program_id}")
        return 0, (time.time() - start_time) * 1000
    except Exception as e:
        logger.error(f"Error computing V(G): {e}")
        return 0, (time.time() - start_time) * 1000
    finally:
        # Clean up temp file
        try:
            os.unlink(temp_file)
        except:
            pass


class DirectComputeCoordinator:
    """Coordinator that computes directly without services"""
    
    def __init__(self, project_root: Optional[Path] = None):
        """
        Initialize direct compute coordinator
        
        Args:
            project_root: Root of the project (auto-detected if None)
        """
        if project_root is None:
            # Auto-detect project root
            current = Path(__file__).resolve()
            # Go up from python-coordinator/coordinator/direct_compute.py
            project_root = current.parent.parent.parent
        
        self.project_root = project_root
        from .validation import HypothesisValidator
        self.validator = HypothesisValidator()
    
    def validate_program(self, program_id: str, source_code: str):
        """Validate using direct computation"""
        from .validation import ValidationResult
        
        try:
            # Compute both values directly
            h1, h2, h3, h4, h1_time = compute_h1_direct(source_code, program_id, self.project_root)
            vg, vg_time = compute_vg_direct(source_code, program_id, self.project_root)
            
            # Validate hypothesis
            result = self.validator.validate_program(program_id, h1, vg)
            result.h2 = h2
            result.h3 = h3
            result.h4 = h4
            result.h1_time_ms = h1_time
            result.vg_time_ms = vg_time
            result.total_time_ms = h1_time + vg_time
            
            return result
            
        except Exception as e:
            logger.error(f"Validation failed for {program_id}: {e}")
            from .validation import ValidationResult
            return ValidationResult(
                program_id=program_id,
                h1=0,
                h2=0,
                h3=0,
                h4=0,
                vg=0,
                k=0,
                difference=0,
                hypothesis_holds=False,
                success=False,
                error=str(e)
            )

