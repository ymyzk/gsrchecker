# A Type Checker for GTLC + shift/reset

[![Build Status](https://travis-ci.org/ymyzk/gsrchecker.svg?branch=master)](https://travis-ci.org/ymyzk/gsrchecker)

A type checker for Gradually-Typed Lambda Calculus with shift/reset.

## Requirements
- OCaml
- ocamlfind
- OMake
- OUnit

## Usage
### Compile
- `omake`

### Run
- `./src/main`

#### Customize
Current implementation **DOES NOT** includes a parser.
If you want to check other terms, please follow the instructions below:

- Open [src/main.ml](src/main.ml)
- Edit `cases`
- Re-Compile & run

### Test
- `omake test`

## License
MIT License. See [LICENSE](LICENSE).

## References
- TBA
