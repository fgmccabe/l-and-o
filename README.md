# L & O
(Logic and Objects)

This is a compiler and run-time for the logic programming language known as L&O.

This is very much a work in progress and the documentation in the Doc directory is currently completely wrong.

## Structure
There are two compilers: a bootstrap compiler - written in Prolog - and a proper compiler - written in L&O itself.

The compiler is currently the best source of documentation for the language.

### Run-time
There is also a run-time - written in C. The run-time is fairly complete but not fully debugged or fleshed out.

Longer term, the run-time will be enhanced with a JIT-based system that generates machine code on the fly from the byte code.

The machine is a modified version of the Warren Abstract Machine - with some special hacks: the WAM does not understand higher-order programming or objects.

## Road Map
1. The compiler must be made stable (including both inlining and predicate pushdown as key optimizations)
1. The language will then be extended to include a concurrency model.
1. An IDE will be developed on top of Atom (in the first instance).
1. Documentation:
  1. Reference manual
  1. Howto program in L&O
1. Maybe change the name.

Some extensions:
1. Auto-documentation of APIs by reading the source + annotations
1. Compiler plug-in architecture to enable language extensions.
