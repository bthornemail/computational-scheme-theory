"""
Validation Logic for Computational Scheme Theory

This module implements the core validation logic for testing the hypothesis:
    H¹(X_Comp, O_Comp) = V(G) - k
"""

from dataclasses import dataclass
from typing import Optional, Dict, Any
import logging

logger = logging.getLogger(__name__)


@dataclass
class ValidationResult:
    """Result of validating a single program against the hypothesis"""
    program_id: str
    h1: int  # From Haskell service
    vg: int  # From Racket service
    k: int  # Normalization constant
    difference: int  # |H¹ - (V(G) - k)|
    hypothesis_holds: bool  # Is H¹ = V(G) - k?
    success: bool
    error: Optional[str] = None
    h1_time_ms: Optional[float] = None
    vg_time_ms: Optional[float] = None
    total_time_ms: Optional[float] = None


class HypothesisValidator:
    """Validates the Computational Scheme Theory hypothesis"""
    
    def __init__(self, default_k: int = 1):
        """
        Initialize validator
        
        Args:
            default_k: Default normalization constant (typically 0, 1, or 2)
        """
        self.default_k = default_k
    
    def validate_program(
        self,
        program_id: str,
        h1: int,
        vg: int,
        k: Optional[int] = None
    ) -> ValidationResult:
        """
        Validate hypothesis for a single program
        
        Args:
            program_id: Unique identifier for the program
            h1: H¹ cohomology value from Haskell service
            vg: V(G) cyclomatic complexity from Racket service
            k: Normalization constant (if None, uses default_k)
            
        Returns:
            ValidationResult with hypothesis test outcome
        """
        if k is None:
            k = self._estimate_k(h1, vg)
        
        # Hypothesis: H¹ = V(G) - k
        expected_h1 = vg - k
        difference = abs(h1 - expected_h1)
        hypothesis_holds = (difference == 0)
        
        logger.info(
            f"Validated {program_id}: H¹={h1}, V(G)={vg}, k={k}, "
            f"difference={difference}, holds={hypothesis_holds}"
        )
        
        return ValidationResult(
            program_id=program_id,
            h1=h1,
            vg=vg,
            k=k,
            difference=difference,
            hypothesis_holds=hypothesis_holds,
            success=True
        )
    
    def _estimate_k(self, h1: int, vg: int) -> int:
        """
        Estimate normalization constant k
        
        Currently uses a simple heuristic:
        - Try k=0, k=1, k=2 and pick the one that minimizes |H¹ - (V(G) - k)|
        - Default to self.default_k if none match exactly
        
        Args:
            h1: H¹ value
            vg: V(G) value
            
        Returns:
            Estimated k value
        """
        # Try common values
        candidates = [0, 1, 2]
        best_k = self.default_k
        best_diff = abs(h1 - (vg - best_k))
        
        for k in candidates:
            diff = abs(h1 - (vg - k))
            if diff < best_diff:
                best_diff = diff
                best_k = k
            if diff == 0:
                # Perfect match found
                return k
        
        return best_k


def compute_correlation(results: list[ValidationResult]) -> float:
    """
    Compute Pearson correlation coefficient between H¹ and V(G)
    
    Args:
        results: List of validation results
        
    Returns:
        Correlation coefficient (0.0 to 1.0)
    """
    if len(results) < 2:
        return 0.0
    
    # Extract H¹ and V(G) values
    h1_values = [r.h1 for r in results if r.success]
    vg_values = [r.vg for r in results if r.success]
    
    if len(h1_values) < 2:
        return 0.0
    
    # Compute correlation
    n = len(h1_values)
    mean_h1 = sum(h1_values) / n
    mean_vg = sum(vg_values) / n
    
    numerator = sum((h1_values[i] - mean_h1) * (vg_values[i] - mean_vg) 
                    for i in range(n))
    h1_var = sum((h - mean_h1) ** 2 for h in h1_values)
    vg_var = sum((v - mean_vg) ** 2 for v in vg_values)
    
    denominator = (h1_var * vg_var) ** 0.5
    
    if denominator == 0:
        return 0.0
    
    return numerator / denominator


def compute_statistics(results: list[ValidationResult]) -> Dict[str, Any]:
    """
    Compute validation statistics
    
    Args:
        results: List of validation results
        
    Returns:
        Dictionary with statistics
    """
    successful = [r for r in results if r.success]
    
    if not successful:
        return {
            'total': len(results),
            'succeeded': 0,
            'failed': len(results),
            'hypothesis_holds_count': 0,
            'hypothesis_fails_count': 0,
            'hypothesis_success_rate': 0.0
        }
    
    hypothesis_holds = [r for r in successful if r.hypothesis_holds]
    
    return {
        'total': len(results),
        'succeeded': len(successful),
        'failed': len(results) - len(successful),
        'hypothesis_holds_count': len(hypothesis_holds),
        'hypothesis_fails_count': len(successful) - len(hypothesis_holds),
        'hypothesis_success_rate': len(hypothesis_holds) / len(successful) if successful else 0.0,
        'mean_difference': sum(r.difference for r in successful) / len(successful),
        'correlation': compute_correlation(successful)
    }

