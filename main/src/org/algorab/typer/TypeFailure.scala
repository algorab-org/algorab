/** Type-checking failures reported by [[Typer]].
  *
  * Each case of [[TypeFailure]] corresponds to a distinct kind of type error that can
  * occur during the type-checking phase.  Failures are collected via Kyo's `Emit` effect
  * and converted to human-readable strings by [[org.algorab.CompilerFailure.toPrettyString]].
  */
package org.algorab.typer

import kyo.Chunk
import org.algorab.CompilerFailure
import org.algorab.ast.Identifier
import org.algorab.ast.tpd
import org.algorab.ast.untpd

/** A type-checking failure.
  *
  * Failures are emitted via `Typing.fail` / `Typing.failAndAbort`; accumulation and
  * error reporting are handled by [[Typing.run]].
  */
enum TypeFailure:
  /** The expression's type does not match any of the expected types.
    *
    * @param got      the actual type of the expression
    * @param expected one or more acceptable types (may be empty if no expectation was available)
    */
  case Mismatch(got: tpd.Type, expected: tpd.Type*)

  /** A variable name was used but is not in scope.
    *
    * @param name the unresolved variable name
    */
  case UnknownVariable(name: Identifier)

  /** A type name was used but is not in scope.
    *
    * @param name the unresolved type name
    */
  case UnknownType(name: Identifier)

  /** A member (field or method) was selected on an instance but does not exist.
    *
    * @param className  the class being accessed
    * @param fieldName  the member name that could not be resolved
    */
  case UnknownMember(className: Identifier, fieldName: Identifier)

  /** A variable was declared with a name that is already bound in the current scope.
    *
    * @param name the duplicate variable name
    */
  case VariableAlreadyDefined(name: Identifier)

  /** A type was declared with a name that is already bound in the current scope.
    *
    * @param name the duplicate type name
    */
  case TypeAlreadyDefined(name: Identifier)

  /** A function call had the wrong number of arguments.
    *
    * @param got      the number of arguments provided by the caller
    * @param size     the number of parameters declared by the function
    */
  case WrongArgumentCount(got: Int, size: Int)

  /** Fewer type arguments were provided than the function requires.
    *
    * @param missing the type parameter names that have no corresponding argument
    */
  case MissingTypeArguments(missing: Chunk[Identifier])

  /** More type arguments were provided than the function accepts.
    *
    * @param got      the excess type arguments as written in source
    * @param expected the type parameter names that are actually declared
    */
  case TooManyTypeArguments(got: Chunk[untpd.Type], expected: Chunk[Identifier])

  /** The type of a variable could not be inferred (e.g. the initialiser itself is unresolved).
    *
    * @param name the variable whose type could not be determined
    */
  case CannotInferType(name: Identifier)

  /** An attempt was made to assign to a variable declared without `mut`.
    *
    * @param name the immutable variable being written to
    */
  case ImmutableVariableAssignment(name: Identifier)

  /** A variable was referenced before its initialiser was type-checked.
    *
    * This can occur with functions that capture variables not yet fully initialised.
    *
    * @param name the variable referenced before initialisation
    */
  case IllegalForwardReference(name: Identifier)

  /** The keyword `this` was used outside of a class body. */
  case ThisOutsideClass
