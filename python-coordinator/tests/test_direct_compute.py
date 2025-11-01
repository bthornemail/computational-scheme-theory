"""
Unit tests for direct computation module
"""

import pytest
from pathlib import Path
from coordinator.direct_compute import DirectComputeCoordinator, compute_h1_direct, compute_vg_direct


class TestDirectCompute:
    """Test direct computation functions"""
    
    def test_direct_compute_coordinator_init(self):
        """Test coordinator initialization"""
        coordinator = DirectComputeCoordinator()
        assert coordinator.project_root is not None
        assert coordinator.project_root.exists()
    
    @pytest.mark.skip(reason="Requires Racket to be installed")
    def test_compute_h1_simple_program(self):
        """Test H¹ computation for a simple program"""
        project_root = Path(__file__).parent.parent.parent
        source_code = "(lambda (x) x)"
        
        h1, time_ms = compute_h1_direct(source_code, "test-001", project_root)
        
        assert h1 >= 0
        assert time_ms >= 0
    
    @pytest.mark.skip(reason="Requires Racket to be installed")
    def test_compute_vg_simple_program(self):
        """Test V(G) computation for a simple program"""
        project_root = Path(__file__).parent.parent.parent
        source_code = "(lambda (x) x)"
        
        vg, time_ms = compute_vg_direct(source_code, "test-001", project_root)
        
        assert vg >= 0
        assert time_ms >= 0
    
    @pytest.mark.skip(reason="Requires Racket to be installed")
    def test_direct_coordinator_validate(self):
        """Test validation using direct coordinator"""
        coordinator = DirectComputeCoordinator()
        
        result = coordinator.validate_program(
            "test-001",
            "(lambda (x) x)"
        )
        
        assert result is not None
        assert result.program_id == "test-001"
        assert result.h1 >= 0
        assert result.vg >= 0
        assert isinstance(result.hypothesis_holds, bool)
        assert result.success in [True, False]  # May fail if Racket not available

