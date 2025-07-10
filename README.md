# ortools-sat

Haskell bindings for building and serializing OR-Tools SAT (CP-SAT) models using `proto3-wire`.

This library provides a type-safe API for encoding `CpModelProto`, `SatParameters`, and `CpSolverRequest` directly into Protocol Buffers bytes, suitable for sending to external OR-Tools' SAT solver.

## Features
- Haskell **partial** representation of CP-SAT variables, constraints, and objectives
- Protocol Buffer encoding via `proto3-wire`
- Compatible with OR-Tools 9.14 solver requests
- Roundtrip tests for encoding/decoding solver responses
- Test suite comparing output against canonical C++-generated protobuf
- Supported features
  - Integer and Boolean variables
  - AllDifferent, Equality NotEqual, MinEquality, MaxEquality, LessOrEqual Enforcement constraints

## High-Level API

This library exposes a tagless-final style interface via the `Sat` type class. It lets you define CP-SAT models in a declarative and composable way, abstracted over the modeling backend.

Use the default `SatModel` monad to build models, then serialize them with `requestByteString` or solve them with `solve`.

```haskell
exampleModel :: Sat m => m ()
exampleModel = do
  x <- newIntegerVariable 0 10
  y <- newIntegerVariable 0 10
  equality x y
  minimise x

main :: IO ()
main = do
  let params = defaultParameters
  response <- solve mySolver (exampleModel :: SatModel ()) params
  print response
```

## Specification

See `test/Spec.hs` for coverage of:

- Variable creation (Boolean, Integer)
- Common constraints (Equality, AllDifferent, MinEquality, etc.)
- Objective definitions
- Full request encoding
- Solver response decoding

## Quick Start

### Prerequisites

- GHC ≥ 9.4
- Cabal ≥ 3.8
- OR-Tools 9.14 (optional, for verifying byte-for-byte matches)

### Build and Test

```bash
cabal update
cabal build
cabal test
```