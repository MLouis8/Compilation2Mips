# Compilation2Mips

**Compilation project**:

What's implemented:
- basic exercice 1 work
- constant progation optimization for Imp2Mips with:
  - basic propagation, ex: 4+3 -> 7 or 5 < 7 -> true
  - more advanced propagation, ex: 1 + 3 * b * 2 + 4 -> 5 + b * 6

What I plan to implement:
- naive register allocation optimization:
  - create a naive interference graph (let's consider every variable in a function interfere)
  - colorize the graph with ocamlgraph
  - if there's a solution assign registers to variables
  - Pros: little optimizaiton, may work on small programs
  - Cons: no heuristic for graph coloring, bad interference graph
- exercice 2
