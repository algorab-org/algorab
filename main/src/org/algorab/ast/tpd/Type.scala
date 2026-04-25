/**
 * Typed (resolved) type representations, produced by the type-checker.
 *
 * Every `tpd.Type` is the result of resolving an [[org.algorab.ast.untpd.Type]] against the
 * current [[org.algorab.typer.TypeContext]].  Unlike `untpd.Type`, these values carry fully
 * resolved class names and generic-parameter bindings.
 */
package org.algorab.ast.tpd

import java.util.Objects
import kyo.Chunk
import org.algorab.ast.Identifier

/**
 * A fully resolved type in the Algorab type system.
 *
 * Key distinctions from [[org.algorab.ast.untpd.Type]]:
 *   - `Nothing` is the bottom type (used as the initial element for `union` computations).
 *   - `Generic` holds the ''unique'' internal name assigned by the typer, not the source name.
 *   - `Class` bundles the class name with its constructor type.
 *   - `Instance` bundles the class name with its current generic-parameter substitution map.
 *   - `Inferred` survives type-checking only temporarily; all `Inferred` nodes are expected to
 *     be replaced before code generation.
 */
enum Type derives CanEqual:
  /** The bottom type – no value has this type; used as a neutral element in `union`. */
  case Nothing

  /** Placeholder for a type that is yet to be inferred. */
  case Inferred

  /**
   * A generic type parameter, referenced by its unique internal name.
   *
   * @param name the typer-assigned unique name (e.g. `A`, `A$1`)
   */
  case Generic(name: Identifier)

  /**
   * The type of a class value (i.e. the class constructor itself).
   *
   * @param name        the internal class name
   * @param constructor the type of the constructor function
   */
  case Class(name: Identifier, constructor: Type)

  /**
   * The type of an instance of a class, with its generic substitutions resolved.
   *
   * @param name         the internal class name
   * @param typeParams   the ordered list of generic-parameter names declared by the class
   * @param replacements a mapping from generic-parameter names to the concrete types
   *                     they have been substituted with in this instance
   */
  case Instance(name: Identifier, typeParams: Chunk[Identifier], replacements: Map[Identifier, Type])

  /**
   * A generic type application (e.g. `Array[Int]`).
   *
   * @param base the base (higher-kinded) type
   * @param args the type arguments
   */
  case Apply(base: Type, args: Chunk[Type])

  /**
   * A monomorphic function type.
   *
   * @param params the parameter types
   * @param output the return type
   */
  case Fun(params: Chunk[Type], output: Type)

  /**
   * A polymorphic function type (type-level lambda).
   *
   * @param typeParams the names of the type parameters
   * @param output     the body type, which may reference the type parameters
   */
  case TypeFun(typeParams: Chunk[Identifier], output: Type)

  /**
   * A product (tuple) type.
   *
   * @param elements the component types, in order
   */
  case Tuple(elements: Chunk[Type])

  /**
   * Creates a two-element tuple type from `this` and `other`.
   *
   * @param other the second element type
   * @return `Tuple(Chunk(this, other))`
   */
  def zip(other: Type): Type = Type.Tuple(Chunk(this, other))

  /**
   * Returns `other` when this type is [[Inferred]], otherwise returns `this`.
   *
   * Useful when a fallback concrete type should override an as-yet-uninferred one.
   *
   * @param other the fallback type
   * @return `this` if not [[Inferred]], `other` otherwise
   */
  def notInferredOr(other: Type): Type =
    if this == Inferred then other
    else this

  /**
   * Substitutes every [[Generic]] node whose name appears in `replacements`.
   *
   * Recursively traverses the type tree.  Inside an [[Instance]], the replacements
   * are merged into the existing substitution map rather than applied eagerly.
   *
   * @param replacements a map from generic-parameter names to their concrete substitutes
   * @return a new `Type` tree with all matching generics replaced
   */
  def replaceGeneric(replacements: Map[Identifier, Type]): Type = this match
    case Nothing                             => Type.Nothing
    case Inferred                            => Type.Inferred
    case Generic(n)                          => replacements.getOrElse(n, this)
    case Class(name, constructor)            => Type.Class(name, constructor.replaceGeneric(replacements))
    case Instance(name, typeParams, members) => Type.Instance(name, typeParams, members.map((k, v) => (k, v.replaceGeneric(replacements))))
    case Apply(base, args)                   => Type.Apply(base.replaceGeneric(replacements), args.map(_.replaceGeneric(replacements)))
    case Fun(params, output)                 => Type.Fun(params.map(_.replaceGeneric(replacements)), output.replaceGeneric(replacements))
    case TypeFun(typeParams, output)         => Type.TypeFun(typeParams, output.replaceGeneric(replacements))
    case Tuple(elements)                     => Type.Tuple(elements.map(_.replaceGeneric(replacements)))

/** Pre-built type instances for the standard Algorab built-in types. */
object Type:

  // Std types
  val Any: Type = Instance(Identifier("Any"), Chunk.empty, Map.empty)
  val Unit: Type = Instance(Identifier("Unit"), Chunk.empty, Map.empty)
  val Boolean: Type = Instance(Identifier("Boolean"), Chunk.empty, Map.empty)
  val Int: Type = Instance(Identifier("Int"), Chunk.empty, Map.empty)
  val Float: Type = Instance(Identifier("Float"), Chunk.empty, Map.empty)
  val Char: Type = Instance(Identifier("Char"), Chunk.empty, Map.empty)
  val String: Type = Instance(Identifier("String"), Chunk.empty, Map.empty)
  val Array: Type = Instance(Identifier("Array"), Chunk.empty, Map.empty)
  def arrayOf(tpe: Type): Type = Type.Apply(Array, Chunk(tpe))
