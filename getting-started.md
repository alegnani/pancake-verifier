# Getting started guide for Pancake

Pancake is an untyped systems language with a fully verified compiler based on `CakeML` and soonTM support for auto-active style annotations.


To get started grab yourself a copy of the CakeML compiler [here](https://github.com/CakeML/cakeml/releases) and run `make cake` (`make cake.exe` on Windows) to compile and link the cake compiler.

## Compiling a Pancake program

To compile a Pancake program, here `foo.🥞`, run `cake --pancake < foo.🥞 > out.S`. As Pancake only communicates to the outside word via FFI calls we need to link our FFI functions with the generated assembly file.
A basic set of FFIs is provided in the download as `basis_ffi.c`.
This is done with `gcc -DEVAL basis_ffi.c out.S`. (XXX: why is EVAL needed?).

# Extending Pancake with the C precompiler

Given that Pancake is a very simple language a macro system is used to avoid excessive code duplication. For this the C precompiler can be used by running `cat file1.🥞 file2.🥞 | cpp -P > out.🥞`. The generated Pancake file can then be compiled with the cake compiler.

# Language features

Pancake does not have a real type system and instead relies only on the notions of bytes, words and structs.
