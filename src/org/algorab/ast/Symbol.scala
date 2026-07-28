package org.algorab.ast

import io.github.iltotore.pureparser.Span

//TODO use symbol owner instead of qualified name and add "root" symbol.
sealed trait Symbol derives CanEqual:

  def reportName: String = this match
    case Symbol.Variable(_, name, _, _, _) => s"variable $name"
    case Symbol.Function(_, name, _, _) => s"function $name"
    case Symbol.Type(_, name, _, _) => s"type $name"
    case Symbol.Invalid => "<invalid>"

  def owner: Option[SymbolId]
  
object Symbol:

  sealed trait Valid extends Symbol:
    def name: Identifier
    def span: Span

    def withOwner(owner: SymbolId): Symbol.Valid = this match
      case Variable(id, name, _, mutable, span) => Variable(id, name, Some(owner), mutable, span)
      case Function(id, name, _, span) => Function(id, name, Some(owner), span)
      case Type(id, name, _, span) => Type(id, name, Some(owner), span)
    
  case class Variable(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      mutable: Boolean,
      span: Span
  ) extends Valid

  case class Function(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      span: Span
  ) extends Valid

  case class Type(
      id: SymbolId,
      name: Identifier,
      owner: Option[SymbolId],
      span: Span
  ) extends Valid

  case object Root extends Symbol:
    override def owner: Option[SymbolId] = None

  case object Invalid extends Symbol:
    override def owner: Option[SymbolId] = None