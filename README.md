Freak
======

Functional programming language with algebraic effects and handlers, based
on existing [Links langauge](http://homepages.inf.ed.ac.uk/slindley/papers/handlers-cps.pdf).
Implemented by doing continuation passing style tranformation.

# Usage guide
All commands are available within src directory.

## Build and install
- Install dependencies: `make install`
- Compile: `make build`
- Link to PATH: `sudo make link`

- Remove artiacts: `make clean`
- Run tests: `make tests`
- Run code linter: `make lint`

After compiling and linking program to PATH, one may evaluate program
as follows: `freak programs/choicesList.fk`

## Running tests

Test cases are available [here](./src/Tests.hs), they include both inline and file-based tests. For
more details about writing tests, one may refer to [HUnit documentation](https://hackage.haskell.org/package/HUnit).

- Run tests: `make tests`
- Run code linter: `make lint`
- Compile, run linter and tests: `make check`
