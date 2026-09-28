package org.algorab.ast

import org.algorab.util.SourcePosition

/**
 * A symbol represents a declared member like a variable or a function.
 */
sealed trait Symbol derives CanEqual:

  /**
   * The namespace owner of this symbol.
   */
  def owner: Option[SymbolId]

object Symbol:

  /**
   * A valid symbol.
   */
  sealed trait Valid extends Symbol:

    /**
     * The name of this symbol, usually used for reporting purpose.
     */
    def name: Identifier

    /**
     * The source position of this symbol's declaration.
     */
    def position: SourcePosition

    /**
     * Set the owner of this symbol.
     *
     * @param owner the new owner of this symbol
     * @return a copy of this symbol with the new owner
     */
    def withOwner(owner: SymbolId): Symbol.Valid = this match
      case Variable(id, name, _, mutable, position) => Variable(id, name, Some(owner), mutable, position)
      case Function(id, name, _, position)          => Function(id, name, Some(owner), position)
      case Type(id, name, _, position)              => Type(id, name, Some(owner), position)
      case _                                        => throw AssertionError(s"withOwner with $this")

  /**
   * A symbol that can be referenced by a namespace.
   */
  sealed trait Namespace extends Symbol, Valid:

    /**
     * The scope containing this symbol's members.
     */
    def memberScope: ScopeId

  /**
   * A variable.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param mutable whether or not this variable is mutable
   * @param position the source position of this symbol's declaration
   */
  case class Variable(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      mutable: Boolean,
      position: SourcePosition
  ) extends Valid

  /**
   * A function.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param position the source position of this symbol's declaration
   */
  case class Function(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      position: SourcePosition
  ) extends Valid

  /**
   * A type.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param position the source position of this symbol's declaration
   */
  case class Type(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      position: SourcePosition
  ) extends Valid

  /**
   * A package.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param memberScope the scope containing this symbol's members
   */
  case class Package(id: SymbolId, name: Identifier, owner: Option[SymbolId], memberScope: ScopeId) extends Namespace:
    override def position: SourcePosition = SourcePosition.BuiltIn

  /**
   * The root symbol, ancestor of all symbols.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param memberScope the scope containing this symbol's members
   */
  case object Root extends Namespace:
    override def name: Identifier = Identifier("root")
    override def owner: Option[SymbolId] = None
    override def memberScope: ScopeId = ScopeId.Root
    override def position: SourcePosition = SourcePosition.BuiltIn

  /**
   * An invalid symbol.
   */
  case object Invalid extends Symbol:
    override def owner: Option[SymbolId] = None
