/** Untyped type representations, as produced directly by the parser.
  *
  * These types mirror the syntactic forms available in Algorab source code.
  * They are '''not''' resolved – a `Ref` is just a name, with no guarantee that it
  * refers to a known type.  Resolution happens during type-checking (see
  * [[org.algorab.typer.Typer.resolveType]]), which converts each `untpd.Type` into
  * the corresponding [[org.algorab.ast.tpd.Type]].
  */
package org.algorab.ast.untpd

import kyo.Chunk
import org.algorab.ast.Identifier

/** An unresolved type expression as written in Algorab source code.
  *
  * The variants closely follow the syntactic grammar:
  *   - `Inferred` appears wherever the user omits the type annotation (e.g. `val x = 1`).
  *   - `Ref` is a bare type name such as `Int` or `MyClass`.
  *   - `Apply` represents a generic application such as `Array[Int]`.
  *   - `Fun` is a function type such as `Int => String`.
  *   - `TypeFun` is a polymorphic (type-parametric) function type.
  *   - `Tuple` is a product type written as `(A, B, C)`.
  */
enum Type derives CanEqual:
  /** Placeholder used when the type is to be inferred by the type-checker. */
  case Inferred

  /** A reference to a named type (e.g. `Int`, `String`, `MyClass`).
    *
    * @param name the bare type name as it appears in source
    */
  case Ref(name: Identifier)

  /** A generic type application (e.g. `Array[Int]`).
    *
    * @param base the type constructor (e.g. `Array`)
    * @param args the type arguments (e.g. `[Int]`)
    */
  case Apply(base: Type, args: Chunk[Type])

  /** A function type (e.g. `Int => String` or `(Int, Float) => Boolean`).
    *
    * @param params the parameter types
    * @param output the return type
    */
  case Fun(params: Chunk[Type], output: Type)

  /** A polymorphic function type with explicit type parameters (e.g. `[A] => A => A`).
    *
    * @param typeParams the names of the type parameters
    * @param output     the body type, which may reference the type parameters
    */
  case TypeFun(typeParams: Chunk[Identifier], output: Type)

  /** A tuple type (e.g. `(Int, String)`).
    *
    * @param elements the component types, in order
    */
  case Tuple(elements: Chunk[Type])

  /** Creates a two-element tuple type from `this` and `other`.
    *
    * @param other the second element type
    * @return `Tuple(Chunk(this, other))`
    */
  def zip(other: Type): Type = Type.Tuple(Chunk(this, other))

  /** Returns `other` when this type is [[Inferred]], otherwise returns `this`.
    *
    * Useful for supplying a fallback when a type annotation was omitted.
    *
    * @param other the fallback type
    * @return `this` if not [[Inferred]], `other` otherwise
    */
  def notInferredOr(other: Type): Type =
    if this == Inferred then other
    else this

/** Predefined type aliases for the standard Algorab built-in types. */
object Type:

  /** The top type; every Algorab type is a subtype of `Any`. */
  val Any: Type = Ref(Identifier("Any"))

  /** The unit type, used for expressions that produce no meaningful value. */
  val Unit: Type = Ref(Identifier("Unit"))

  /** Boolean type (`true` / `false`). */
  val Boolean: Type = Ref(Identifier("Boolean"))

  /** 32-bit signed integer type. */
  val Int: Type = Ref(Identifier("Int"))

  /** 64-bit IEEE 754 double-precision floating-point type. */
  val Float: Type = Ref(Identifier("Float"))

  /** Unicode character type. */
  val Char: Type = Ref(Identifier("Char"))

  /** Immutable string type. */
  val String: Type = Ref(Identifier("String"))

  /** Generic array type constructor (unparameterised). */
  val Array: Type = Ref(Identifier("Array"))
