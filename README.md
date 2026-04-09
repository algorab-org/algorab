# Algorab

Algorab is a programming language designed to meet the educational needs non-fulfilled by languages such as Python or C in higher education for teaching programming and algorithms. Using features such as strong static typing, meaningful error messages, algebraic data types and effect handlers, Algorab promotes effective learning of algorithms and good programming practices.

> [!WARNING]
> Algorab is still in very early development and not all advertised features are implemented yet.

## Installation

You can use the [released artifacts](https://github.com/algorab-org/algorab/releases) (JAR or native executable).

You can also run the project from source using:

```
./millw cli.run <args>
```

Note: requires JDK 17+.

For Windows, use `millw.bat` instead of `millw`.

## Usage

```
algorab [--help] [--version] <path>
```

Example:

```
algorab my_program.algo
```

## Building the project

The project can be compiled using the following command:

```
./millw <module>.compile
```

where `<module>` is either `main`, `main.test` or `cli`. Since `cli` depends on `main`, compiling `cli` also compiles `main`.

The tests can be run using:

```
./millw main.test
```

Finally, you can build the JAR (Java bytecode) artifact (stored in `out/cli/assembly.dest/`) using:

```
./millw cli.assembly
```

## Internals

### Ecosystem

We used the [Scala programming language](https://scala-lang.org/) for building Algorab. Its FP capabilities including ADTs, pattern matching, and its vast ecosystem of parsing libraries make it well-suited for compiler development. The build tool used for this project is [Mill](https://mill-build.com/).

We built Algorab upon [Kyo](https://getkyo.io/), an ecosystem based on effect handlers providing the guarantees of monads while vastly improving composability.

Used libraries:

- [Kyo](https://getkyo.io/): effect handlers
- Kyo Direct: alternative "coroutine-like" syntax for Kyo
- Kyo Parse: parser combinators, used for the lexer and syntactic parser
- [Iron](https://github.com/Iltotore/iron/): refined types to strengthen the typing system and prevent more bugs at compile-time
- [Decline](https://ben.kirw.in/decline/) (CLI module only): composable command-line parsing

### Compilation

The compiler contains multiple phases from textual source code to program execution:

- Lexing: turn the source code (`String`) to a list of tokens
- Parsing: build an untyped Abstract Syntax Tree (AST) from the tokens
- Typing: elaborate the untyped AST into a typed AST, checking that all types, identifiers and functions are valid
- Compilation: compile the typed AST into a flat sequence of instructions
- VM execution: interpret and execute the instructions