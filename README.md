rosette-template
===

A `#lang rosette` template for program verification and synthesis.

## `from-template` installation

You can skip this section if you have already done it.

1. [Set your PATH environment variable](https://github.com/racket/racket/wiki/Set-your-PATH-environment-variable) 
   so that you can use Racket command-line functions.
2. Install `from-template` either from the DrRacket menu **File | Package Manager** 
   or from the command `raco pkg install from-template`.

## `rosette-template` installation 

Run the command:

```bash
raco new rosette-template <destination-dir>
# if you omit `<destination-dir>`, the default is `./rosette-template`
```

## How to use

The template contains many Rosette example files. 

- `synth.rkt`: a sample program synthesizer (synthesis query) taken from 
   [_Building a Program Synthesizer_](https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html)
   by James Bornholt.
- `verify.rkt`: a sample program verifier (verification query).
- `sudoku.rkt`: a sample Sudoku solver (angelic execution query)

Detailed description is given in each file.
