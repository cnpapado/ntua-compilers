# Compiler for the Grace language

This repo provides an implementation of a Grace language compiler in OCaml, for the Compilers NTUA course, using **ocamllex** for lexical analysis, **menhir** for syntactic analysis and **LLVM** for code generation. 
Additionally, the project includes AST, IR and final code optimizations, and a runtime library written in C.


## Installation and usage

To install and build the compiler, the following tools must be pre-installed:

- `OCaml`
- `clang`
- `ar`
- `llvm` and `llvm-dev`
- `opam` and `dune`

Install necessary `opam` packages by running:
```
opam install dune menhir core core_unix llvm
```

### Building:
- Navigate to the `grace_compiler` directory
- If `opam` is not set up in your environment, run:
```
eval $(opam config env)
```
- Run `make`. This will compile both the compiler and the runtime library which is located in the `runtime_library/` dir.

To remove all build files use:
```
make clean
```
or to remove all generated files use:
```
make disclean
```

After building, the executable `gracec.exe` will be created in the `grace_compiler/` directory. Run  
```
./gracec {<file>|-i|-f} [-O]
```
Use `gracec.exe -h` for a more detailed description of various command line arguments and flags it accepts.



## Notes on design and implementation

### AST(s) as functors
Each compiling stage (from semantic analysis and onwards, that is semantic analysis, ast
optimizations, ir code generation) accepts and returns an AST. I wanted to be able to store different metadata for the nodes of each AST variant
(e.g. AST with node location information for syntactic analysis, typed AST for semantic analysis, etc) but with sharing the same structure, to eliminate code reuse (re-writting the AST structure in each stage). 
Therefore I implemented the AST structure as a functor that takes a parameterized type and generates an AST with that
type as metadata. Thus each AST variant is a module that results as an implementation
of that functor with a different metadata type.

### Semantic analysis
I use a (mutable) symbol table that stores function types, parameter types
and variables when declaring them and which I call/search for during type checking.
Indexing of constant strings (eg "hi"[1]) and indexing arrays with characters (eg a['a']) are prohibited. 
I am also not checking that array indexes which can be evaluated at compile time
are within array bounds. Also, I require array actual parameters to have the same size as the formal ones. An exception is the case where a
array as an actual parameter appears incomplete in the declaration but complete in the
definition, where I allow the formal parameter to be of a different size.

### LLVM IR generation
In Grace the top-level function of the program can have any name (not
necessarily main), so I wrap it in a function with the name `main` which subsequently calls
the top-level and returns 0. Because LLVM requires unique function names,
I rename all functions with unique names during AST optimizations
(see below).

Variables are allocated on stack and thus mutability of these is achieved. In
function definitions, the formal parameters are also copied to the stack (during first reference) 
so that they can be mutatable.

A symbol table is also used at this stage to store the LLVM
function units (which need to be retrieved in function calls) and the memory locations of
variables.

By-reference parameters are implemented as pointers and tables as pointers to (llvm)
tables (same as multidimensional tables). Incomplete arrays are implemented as
pointers to their first element.

Because every LLVM basic block must terminate, I add a return statement at the end
of any function that does not terminate with return (this can happen with function
returns within `if` cases).

### Lambda lifting and other optimizations
I implement lambda lifting in the AST before code generation and I eliminate nested functions
by converting them to global, passing all free variables as by-reference parameters. This, if combined with
LLVM's reg2mem and constant folding passes can create further opportunities for
optimization, but also serves a more practical purpose as in LLVM it is not possible to
access the activation records of the parent functions and therefore a nested function cannot access a variable in the scope of an external function (nested scoping).

At the final code stage, we use the following LLVM optimization passes:
- memory_to_register_promotion (very useful given how we implement variables)
- constant_propagation (similarly)
- reassociation (facilitates better constant propagation)
- global value numbering (also eliminates redundant loads)
- dead code elimination
- function_inlining

