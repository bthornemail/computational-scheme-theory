"""
Unit tests for validation logic
"""

import pytest
from coordinator.validation import (
    ValidationResult,
    HypothesisValidator,
    compute_correlation,
    compute_statistics
)


class TestHypothesisValidator:
    """Test hypothesis validation logic"""
    
    def test_perfect_match(self):
        """Test validation when H¹ = V(G) - k exactly"""
        validator = HypothesisValidator(default_k=1)
        result = validator.validate_program("test-1", h1=2, vg=3, k=1)
        
        assert result.hypothesis_holds is True
        assert result.difference == 0
        assert result.h1 == 2
        assert result.vg == 3
        assert result.k == 1
    
    def test_mismatch(self):
        """Test validation when hypothesis doesn't hold"""
        validator = HypothesisValidator(default_k=1)
        result = validator.validate_program("test-2", h1=5, vg=3, k=1)
        
        assert result.hypothesis_holds is False
        assert result.difference == abs(5 - (3 - 1))  # |5 - 2| = 3
    
    def test_k_estimation(self):
        """Test automatic k estimation"""
        validator = HypothesisValidator(default_k=1)
        
        # Should find k=0
        result = validator.validate_program("test-3", h1=3, vg=3)
        assert result.k == 0
        
        # Should find k=1
        result = validator.validate_program("test-4", h1=2, vg=3)
        assert result.k == 1
        
        # Should find k=2
        result = validator.validate_program("test-5", h1=1, vg=3)
        assert result.k == 2


class TestStatistics:
    """Test statistical computation"""
    
    def test_correlation_perfect(self):
        """Test correlation with perfect linear relationship"""
        results = [
            ValidationResult("1", h1=1, vg=2, k=1, difference=0, hypothesis_holds=True, success=True),
            ValidationResult("2", h1=2, vg=3, k=1, difference=0, hypothesis_holds=True, success=True),
            ValidationResult("3", h1=3, vg=4, k=1, difference=0, hypothesis_holds=True, success=True),
        ]
        corr = compute_correlation(results)
        assert abs(corr - 1.0) < 0.001  # Should be close to 1.0
    
    def test_statistics_computation(self):
        """Test statistics computation"""
        results = [
            ValidationResult("1", h1=1, vg=2, k=1, difference=0, hypothesis_holds=True, success=True),
            ValidationResult("2", h1=3, vg=4, k=1, difference=0, hypothesis_holds=True, success=True),
            ValidationResult("3", h1=5, vg=3, k=1, difference=3, hypothesis_holds=False, success=True),
        ]
        stats = compute_statistics(results)
        
        assert stats['total'] == 3
        assert stats['succeeded'] == 3
        assert stats['hypothesis_holds_count'] == 2
        assert stats['hypothesis_fails_count'] == 1
        assert stats['hypothesis_success_rate'] == pytest.approx(2/3)

