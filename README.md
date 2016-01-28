# A Type Checker for GTLC + shift/reset

[![Build Status](https://travis-ci.org/ymyzk/gsrchecker.svg?branch=master)](https://travis-ci.org/ymyzk/gsrchecker)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.45256.svg)](http://dx.doi.org/10.5281/zenodo.45256)

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

#### Notes
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
