# Compilation2Mips

**Compilation project**:

What's implemented:
- basic exercice 1 work
- constant progation optimization for Imp2Mips with:
  - basic propagation, ex: 4+3 -> 7 or 5 < 7 -> true
  - more advanced propagation, ex: 1 + 3 * b * 2 + 4 -> 5 + b * 6
  - but doesn't work on: 2 + x + x + 5 -> 7 + x + x 

What I plan to implement:
- better constant propagation in one pass
- exercice 2
