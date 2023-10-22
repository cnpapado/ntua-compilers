# ntua-compilers

Run with `dune exec grace <filename>` or `dune test`.

More for cmd line args:
- https://www.brendanlong.com/how-to-use-corecommandparam.html
- https://ocaml.org/p/core/v0.11.1/doc/Core/Command/index.html


### About multidim arrays
```
A[2][100] is:
 * * *    ...   * * *
 * * *    ...   * * *
```
hense `[A[100], A[100]]`


## Installation
```
opam install dune menhir core core_unix
```