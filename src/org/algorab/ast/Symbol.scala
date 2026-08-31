package org.algorab.ast

import io.github.iltotore.pureparser.Span

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
    def span: Span

    /**
     * Set the owner of this symbol.
     *
     * @param owner the new owner of this symbol
     * @return a copy of this symbol with the new owner
     */
    def withOwner(owner: SymbolId): Symbol.Valid = this match
      case Variable(id, name, _, mutable, span) => Variable(id, name, Some(owner), mutable, span)
      case Function(id, name, _, span)          => Function(id, name, Some(owner), span)
      case Type(id, name, _, span)              => Type(id, name, Some(owner), span)
      case _                                    => throw AssertionError(s"withOwner with $this")

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
   * @param span the source position of this symbol's declaration
   */
  case class Variable(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      mutable: Boolean,
      span: Span
  ) extends Valid

  /**
   * A function.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param span the source position of this symbol's declaration
   */
  case class Function(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      span: Span
  ) extends Valid

  /**
   * A type.
   *
   * @param id the id of this symbol
   * @param name the name of this symbol
   * @param owner the owner of this symbol
   * @param span the source position of this symbol's declaration
   */
  case class Type(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      span: Span
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
    override def span: Span = Span(0, 0)

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
    override def span: Span = Span(0, 0)

  /**
   * An invalid symbol.
   */
  case object Invalid extends Symbol:
    override def owner: Option[SymbolId] = None
