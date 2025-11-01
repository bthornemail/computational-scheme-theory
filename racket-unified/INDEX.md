# Documentation Index

## Quick Links

### Getting Started
- [README.md](README.md) - Project overview and quick start
- [QUICK_START.md](QUICK_START.md) - Get up and running fast
- [USAGE.md](USAGE.md) - Detailed usage guide

### Architecture
- [ARCHITECTURE.md](ARCHITECTURE.md) - System design and architecture
- [API Reference](src/api.rkt) - Public API documentation

### Implementation
- [IMPLEMENTATION_STATUS.md](IMPLEMENTATION_STATUS.md) - Current status
- [IMPLEMENTATION_COMPLETE.md](IMPLEMENTATION_COMPLETE.md) - Completion summary
- [PROGRESS_REPORT.md](PROGRESS_REPORT.md) - Progress tracking
- [CHANGELOG.md](CHANGELOG.md) - Version history

### Deployment
- [DEPLOYMENT.md](DEPLOYMENT.md) - Deployment guide
- [PROJECT_SUMMARY.md](PROJECT_SUMMARY.md) - Project overview

### Vision & Achievement
- [VISION_ACHIEVED.md](VISION_ACHIEVED.md) - Vision realization
- [COMPLETION_CERTIFICATE.md](COMPLETION_CERTIFICATE.md) - Completion certification
- [FINAL_REPORT.md](FINAL_REPORT.md) - Final status report

## Documentation by Topic

### For Users
1. Start with [QUICK_START.md](QUICK_START.md)
2. Read [USAGE.md](USAGE.md) for detailed examples
3. Check [API Reference](src/api.rkt) for function docs

### For Developers
1. Read [ARCHITECTURE.md](ARCHITECTURE.md) for system design
2. Review [IMPLEMENTATION_STATUS.md](IMPLEMENTATION_STATUS.md) for current state
3. See source code in `src/` directory

### For Deployers
1. Read [DEPLOYMENT.md](DEPLOYMENT.md)
2. Check [README.md](README.md) for requirements
3. Review service configuration in [USAGE.md](USAGE.md)

### For Researchers
1. Read [VISION_ACHIEVED.md](VISION_ACHIEVED.md) for philosophy
2. Review [ARCHITECTURE.md](ARCHITECTURE.md) for design principles
3. Check [PROJECT_SUMMARY.md](PROJECT_SUMMARY.md) for impact

## File Organization

```
racket-unified/
├── README.md                  # Main entry point
├── QUICK_START.md             # Quick start guide
├── USAGE.md                   # Detailed usage
├── ARCHITECTURE.md             # System design
├── DEPLOYMENT.md              # Deployment guide
├── CHANGELOG.md               # Version history
├── IMPLEMENTATION_STATUS.md   # Current status
├── IMPLEMENTATION_COMPLETE.md # Completion summary
├── PROGRESS_REPORT.md         # Progress tracking
├── PROJECT_SUMMARY.md         # Project overview
├── VISION_ACHIEVED.md         # Vision realization
├── COMPLETION_CERTIFICATE.md  # Completion cert
├── FINAL_REPORT.md            # Final status
└── INDEX.md                   # This file
```

## Quick Reference

### Main Commands
```bash
racket src/main.rkt                    # Run demo
racket src/validation-demo.rkt         # Validation
racket test/run-tests.rkt              # Run tests
racket test/corpus-validation.rkt     # Corpus validation
```

### Key Functions
```racket
(require "src/api.rkt")

(compute-h1-from-source-detailed source)  # Main API
(call-racket-vg source)                   # Racket bridge (optional)
(validate-hypothesis h1 vg)               # Hypothesis test
```

### Key Concepts
- **M-expressions**: Meta-language commands
- **S-expressions**: Object-language events
- **Y/Z combinators**: Fixed-point recursion
- **Algorithms 1-4**: Complete H¹ computation pipeline

## Status

**✅ PRODUCTION READY**

- All components working
- All tests passing
- Complete documentation
- Ready for use

---

*Last updated: 2025-01-31*

