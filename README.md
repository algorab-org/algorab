# Algorab

Algorab is a programming language designed to meet the educational needs non-fulfilled by languages such as Python or C in higher education for teaching programming and algorithms. Using features such as strong static typing, meaningful error messages, algebraic data types and effect handlers, Algorab promotes effective learning of algorithms and good programming practices.

For more information, check https://algorab.org/.

> [!WARNING]
> - Algorab is still in very early development and not all advertised features are implemented yet.
> - This is the MVP in development. It cannot run Algorab programs yet. To run Algorab program, check the [old PoC](https://github.com/algorab-org/algorab/tree/poc).

## Building the project

The project can be compiled using the following command:

```
./millw compile
```

The tests can be run using:

```
./millw test
```

Note: on Windows, use `./millw.bat` instead.

## Internals

### Ecosystem

Algorab is built using the [Scala programming language](https://scala-lang.org/) for multiple reasons including its FP capabilities including ADTs, pattern matching. The build tool used for this project is [Mill](https://mill-build.com/).

While the first PoC was made using [Kyo](https://getkyo.io/), Algorab now uses [PureLogic](https://ghostdogpr.github.io/purelogic/), a library based on effect handlers providing the guarantees of monads while vastly improving composability. It is shorter-scoped and simpler than Kyo but far more ergonomic while not degrading Scala error messages.

Used libraries:

- [PureLogic](https://ghostdogpr.github.io/purelogic/): effect handlers
- [PureParser](https://github.com/Iltotore/pureparser): parser combinators, used for the lexer and syntactic parser
- [Iron](https://github.com/Iltotore/iron/): refined types to strengthen the typing system and prevent more bugs at compile-time

### Compilation

The compiler contains multiple phases from textual source code to program execution:

- Lexing: turn the source code (`String`) to a list of tokens
- Parsing: build an raw Abstract Syntax Tree (AST) from the tokens
- Name resolution: resolve all textual references to targetted symbols and handle multi-files
- Typing: elaborate the resolved AST into a typed AST, checking that all types are valid
- Compilation: compile the typed AST into a flat sequence of instructions
- VM execution: interpret and execute the instructions