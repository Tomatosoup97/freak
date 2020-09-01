Freak
======

Experimental functional programming language with (co)algebraic effects and
(co)handlers, implemented by doing continuation passing style tranformation.

CPS translation for algebraic effects in the language is based on existing
[Links langauge](http://homepages.inf.ed.ac.uk/slindley/papers/handlers-cps.pdf),
and coalgebraic part on [Runners in Action](https://arxiv.org/pdf/1910.11629.pdf).

For an introduction to algebraic effects, see resources in
[effects-bibliography](https://github.com/yallop/effects-bibliography).

This language is implemented as a part of thesis, which can be found
[here](./thesis/thesis.pdf). In that document you can find everything about
semantics of the language, implementation details, as well as solid mathematical
foundations.

# Usage guide
All commands are available within src directory.

## Build and install
- Install dependencies: `make install`
- Compile: `make build`
- Link to PATH: `sudo make link`
- Remove artiacts: `make clean`

After compiling and linking program to PATH, one may evaluate program
as follows: `freak <path-to-file>`. To display all usage options, type `freak --help`.
Variety of examples in Freak language can be found in [this](./src/programs) directory.

## Running tests

Test cases are available [here](./src/Tests.hs), they include both inline and file-based tests. For
more details about writing tests, one may refer to [HUnit documentation](https://hackage.haskell.org/package/HUnit).

- Run tests: `make tests`
- Run code linter: `make lint`
- Compile, run linter and tests: `make check`
