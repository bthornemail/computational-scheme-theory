#!/usr/bin/env python3
"""
Setup configuration for Python Coordinator Service
"""

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

with open("requirements.txt", "r", encoding="utf-8") as fh:
    requirements = [line.strip() for line in fh if line.strip() and not line.startswith("#")]

setup(
    name="computational-scheme-coordinator",
    version="0.1.0",
    author="Computational Scheme Theory Research Team",
    author_email="research@example.com",
    description="Python coordinator service for Computational Scheme Theory validation",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/your-org/computational-scheme-theory",
    packages=find_packages(),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Scientific/Engineering :: Mathematics",
    ],
    python_requires=">=3.9",
    install_requires=requirements,
    entry_points={
        "console_scripts": [
            "coordinator=coordinator.service:main",
        ],
    },
)

