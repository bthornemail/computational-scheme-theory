"""
Integration tests for end-to-end validation pipeline
"""

import pytest
from coordinator.service import ValidationCoordinator, MathCoreClient, MetricsClient
from coordinator.validation import ValidationResult


class TestIntegration:
    """Integration tests (require services to be running)"""
    
    @pytest.mark.skip(reason="Requires running services")
    def test_end_to_end_validation(self):
        """Test complete validation pipeline"""
        coordinator = ValidationCoordinator()
        
        test_programs = [
            {
                "program_id": "test-001",
                "source_code": "(lambda (x) x)"
            },
            {
                "program_id": "test-002",
                "source_code": "(if (> x 0) 1 -1)"
            }
        ]
        
        results = coordinator.validate_corpus(test_programs)
        
        assert len(results) == 2
        assert all(r.success for r in results)
    
    @pytest.mark.skip(reason="Requires running services")
    def test_single_program_validation(self):
        """Test validation of a single program"""
        coordinator = ValidationCoordinator()
        
        result = coordinator.validate_program(
            "test-001",
            "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
        )
        
        assert result.success
        assert result.h1 >= 0
        assert result.vg >= 0
        assert isinstance(result.hypothesis_holds, bool)


class TestServiceClients:
    """Test service client implementations"""
    
    def test_math_core_client_init(self):
        """Test math core client initialization"""
        client = MathCoreClient("localhost", 50051)
        client.connect()
        assert client.host == "localhost"
        assert client.port == 50051
    
    def test_metrics_client_init(self):
        """Test metrics client initialization"""
        client = MetricsClient("localhost", 50052)
        client.connect()
        assert client.host == "localhost"
        assert client.port == 50052
    
    @pytest.mark.skip(reason="Requires running service")
    def test_metrics_client_compute_vg(self):
        """Test V(G) computation via HTTP"""
        client = MetricsClient()
        client.connect()
        vg, time_ms = client.compute_vg("(lambda (x) x)", "test-001")
        assert vg >= 0
        assert time_ms >= 0

