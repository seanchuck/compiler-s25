Testing infrastructure for the compiler

Phase 1: Scanner

- run './testscan.sh' to run all gradescope tests


Phase 1: Parser

- run './testparse.sh' to run all gradescope tests


Phase 2: Semantic checker

- run 'cargo test' for local tests
- run './testsemantics.sh' for gradescope tests

Phase 3: Codegen

- run './testcodegen' for local and gradescope tests

Phase 4: Dataflow Optimizations

- run './testopt' for local and gradescope tests with optimizations on.
Use the -O flag to specify optimizations, otherwise defaults to all.


Phase 5: Register Allocation

- run './testopt' for local and gradescope tests with optimizations on.
Use the -O flag to specify optimizations, otherwise defaults to all.
