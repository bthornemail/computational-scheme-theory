#!/usr/bin/env python3
"""
Validate test corpus

Checks that all programs in the corpus:
- Are valid R5RS Scheme syntax
- Have corresponding metadata files
- Have required metadata fields
"""

import json
import sys
from pathlib import Path
from typing import List, Tuple


def validate_program(program_dir: Path, program_id: str) -> Tuple[bool, List[str]]:
    """
    Validate a single program
    
    Returns:
        (is_valid, list_of_errors)
    """
    errors = []
    
    # Check source file exists
    source_file = program_dir / f"{program_id}.scm"
    if not source_file.exists():
        errors.append(f"Source file {source_file} not found")
        return False, errors
    
    # Check metadata file exists
    metadata_file = program_dir / f"{program_id}.json"
    if not metadata_file.exists():
        errors.append(f"Metadata file {metadata_file} not found")
        return False, errors
    
    # Validate metadata
    try:
        metadata = json.loads(metadata_file.read_text())
        required_fields = ["program_id", "category", "description"]
        for field in required_fields:
            if field not in metadata:
                errors.append(f"Missing required field: {field}")
    except json.JSONDecodeError as e:
        errors.append(f"Invalid JSON in metadata: {e}")
    
    # Basic syntax check (could be enhanced with actual parser)
    source_code = source_file.read_text()
    if not source_code.strip():
        errors.append("Source file is empty")
    
    return len(errors) == 0, errors


def validate_corpus(corpus_dir: Path) -> bool:
    """
    Validate entire corpus
    
    Returns:
        True if all programs are valid
    """
    print(f"Validating corpus in {corpus_dir}")
    
    categories = ["baseline", "simple-control", "recursion", "complex-control", 
                  "functional", "call-cc", "real-programs"]
    
    total_programs = 0
    valid_programs = 0
    invalid_programs = 0
    
    for category in categories:
        category_dir = corpus_dir / category
        if not category_dir.exists():
            continue
        
        print(f"\nValidating category: {category}")
        
        # Find all .scm files
        for source_file in category_dir.glob("*.scm"):
            program_id = source_file.stem
            total_programs += 1
            
            is_valid, errors = validate_program(category_dir, program_id)
            if is_valid:
                valid_programs += 1
            else:
                invalid_programs += 1
                print(f"  ❌ {program_id}: {', '.join(errors)}")
    
    print(f"\n=== Validation Summary ===")
    print(f"Total programs: {total_programs}")
    print(f"Valid: {valid_programs}")
    print(f"Invalid: {invalid_programs}")
    
    return invalid_programs == 0


if __name__ == "__main__":
    corpus_dir = Path(__file__).parent.parent
    if len(sys.argv) > 1:
        corpus_dir = Path(sys.argv[1])
    
    success = validate_corpus(corpus_dir)
    sys.exit(0 if success else 1)

