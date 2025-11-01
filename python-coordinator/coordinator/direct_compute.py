"""
Direct Computation Module

This module provides direct computation of H¹ and V(G) without requiring
services to be running. It uses subprocess calls to execute Haskell and Racket
programs directly.
"""

import subprocess
import tempfile
import os
from pathlib import Path
from typing import Tuple, Optional
import logging

logger = logging.getLogger(__name__)


def compute_h1_direct(source_code: str, program_id: str, project_root: Path) -> Tuple[int, float]:
    """
    Compute H¹ directly by calling Haskell executable
    
    Args:
        source_code: Scheme source code
        program_id: Program identifier
        project_root: Root of the project
        
    Returns:
        Tuple of (h1_value, computation_time_ms)
    """
    import time
    start_time = time.time()
    
    # Write source to temporary file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.scm', delete=False) as f:
        f.write(source_code)
        temp_file = f.name
    
    try:
        # Use cabal run instead of finding executable path
        haskell_dir = project_root / "haskell-core"
        
        # Call via cabal run (handles finding executable automatically)
        result = subprocess.run(
            ["cabal", "run", "computational-scheme-theory", "--", "compute-h1", temp_file],
            capture_output=True,
            text=True,
            timeout=30,
            cwd=haskell_dir
        )
        
        if result.returncode == 0:
            # Parse output: "H¹(X_Comp, O_Comp) = 2"
            output = result.stdout.strip()
            try:
                h1 = int(output.split("=")[-1].strip())
                elapsed_ms = (time.time() - start_time) * 1000
                logger.info(f"Computed H¹={h1} for {program_id}")
                return h1, elapsed_ms
            except (ValueError, IndexError):
                logger.warning(f"Could not parse H¹ from output: {output}")
        
        logger.warning(f"Haskell computation failed: {result.stderr}")
        return 0, (time.time() - start_time) * 1000
        
    except FileNotFoundError:
        logger.warning("cabal not found, using placeholder")
        return 0, (time.time() - start_time) * 1000
    except subprocess.TimeoutExpired:
        logger.error(f"Haskell computation timed out for {program_id}")
        return 0, (time.time() - start_time) * 1000
    except Exception as e:
        logger.error(f"Error computing H¹: {e}")
        return 0, (time.time() - start_time) * 1000
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
        racket_dir = project_root / "racket-metrics"
        
        # Create a Racket script to compute V(G)
        racket_script = f"""
#lang racket
(require "cyclomatic.rkt"
         "cfg-builder.rkt"
         "r5rs-parser.rkt")

(define source (file->string "{temp_file}"))
(define ast-list (parse-r5rs source))
(when (not (null? ast-list))
  (define cfg (build-cfg (first ast-list)))
  (define metrics (compute-cyclomatic-complexity cfg))
  (displayln (complexity-metrics-v-g metrics)))
"""
        
        # Write Racket script
        with tempfile.NamedTemporaryFile(mode='w', suffix='.rkt', delete=False) as script_file:
            script_file.write(racket_script)
            script_path = script_file.name
        
        try:
            # Run Racket
            result = subprocess.run(
                ["racket", script_path],
                cwd=racket_dir,
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
            try:
                os.unlink(script_path)
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
            h1, h1_time = compute_h1_direct(source_code, program_id, self.project_root)
            vg, vg_time = compute_vg_direct(source_code, program_id, self.project_root)
            
            # Validate hypothesis
            result = self.validator.validate_program(program_id, h1, vg)
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
                vg=0,
                k=0,
                difference=0,
                hypothesis_holds=False,
                success=False,
                error=str(e)
            )

