# Changelog

## Unreleased

- Versioned compiler state artifacts with pre-deserialization size limits and
  process-coordinated, bounded output-only build caches.
- Compiler state format version 2 rejects version 1 after semantic constraints
  moved to closed variants with an incompatible Marshal layout.
- Serialized compiler sessions for safe concurrent compiler and language-server
  use.
- Stable compiler facade with structured diagnostic codes and phases.
- Smaller documented dynamic boundaries and closed scalar builtin dispatch.
- Warning-clean Native and Melange compatibility test gates.
