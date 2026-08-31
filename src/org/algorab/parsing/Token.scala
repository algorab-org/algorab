package org.algorab.parsing

import io.github.iltotore.pureparser.Span
import org.algorab.ast.Identifier

/**
 * A token, a "word" in the source code.
 */
enum Token derives CanEqual:

  /**
   * A boolean literal.
   *
   * @param value the literal value
   * @param span the source position of this token
   */
  case LBool(value: Boolean, span: Span)

  /**
   * An integer literal.
   *
   * @param value the literal value
   * @param span the source position of this token
   */
  case LInt(value: Int, span: Span)

  /**
   * A float literal.
   *
   * @param value the literal value
   * @param span the source position of this token
   */
  case LFloat(value: Double, span: Span)

  /**
   * A character literal.
   *
   * @param value the literal value
   * @param span the source position of this token
   */
  case LChar(value: Char, span: Span)

  /**
   * A string literal.
   *
   * @param value the literal value
   * @param span the source position of this token
   */
  case LString(value: String, span: Span)

  /**
   * An identifier.
   *
   * @param identifier the referenced name
   * @param span the source position of this token
   */
  case Ident(identifier: Identifier, span: Span)

  /**
   * An indentation token, similar to `{` in brace-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param span the source position of this token
   */
  case Indent(span: Span)

  /**
   * An de-indentation token, similar to `}` in brace-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param span the source position of this token
   */
  case DeIndent(span: Span)

  /**
   * An newline token, similar to `;` in semicolon-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param span the source position of this token
   */
  case Newline(span: Span)

  // Symbols

  /**
   * The `(` symbol.
   *
   * @param span the source position of this token
   */
  case ParenOpen(span: Span)

  /**
   * The `)` symbol.
   *
   * @param span the source position of this token
   */
  case ParenClosed(span: Span)

  /**
   * The `,` symbol.
   *
   * @param span the source position of this token
   */
  case Comma(span: Span)

  /**
   * The `:` symbol.
   *
   * @param span the source position of this token
   */
  case Colon(span: Span)

  /**
   * The `.` symbol.
   *
   * @param span the source position of this token
   */
  case Dot(span: Span)

  /**
   * The `+` symbol.
   *
   * @param span the source position of this token
   */
  case Plus(span: Span)

  /**
   * The `-` symbol.
   *
   * @param span the source position of this token
   */
  case Minus(span: Span)

  /**
   * The `*` symbol.
   *
   * @param span the source position of this token
   */
  case Mul(span: Span)

  /**
   * The `/` symbol.
   *
   * @param span the source position of this token
   */
  case Div(span: Span)

  /**
   * The `//` symbol.
   *
   * @param span the source position of this token
   */
  case IntDiv(span: Span)

  /**
   * The `%` symbol.
   *
   * @param span the source position of this token
   */
  case Percent(span: Span)

  /**
   * The `=` symbol.
   *
   * @param span the source position of this token
   */
  case Equal(span: Span)

  /**
   * The `==` symbol.
   *
   * @param span the source position of this token
   */
  case EqualEqual(span: Span)

  /**
   * The `!=` symbol.
   *
   * @param span the source position of this token
   */
  case NotEqual(span: Span)

  /**
   * The `<` symbol.
   *
   * @param span the source position of this token
   */
  case Less(span: Span)

  /**
   * The `<=` symbol.
   *
   * @param span the source position of this token
   */
  case LessEqual(span: Span)

  /**
   * The `>` symbol.
   *
   * @param span the source position of this token
   */
  case Greater(span: Span)

  /**
   * The `>=` symbol.
   *
   * @param span the source position of this token
   */
  case GreaterEqual(span: Span)

  // Keywords

  /**
   * The `and` symbol.
   *
   * @param span the source position of this token
   */
  case And(span: Span)

  /**
   * The `or` symbol.
   *
   * @param span the source position of this token
   */
  case Or(span: Span)

  /**
   * The `not` symbol.
   *
   * @param span the source position of this token
   */
  case Not(span: Span)

  /**
   * The `if` symbol.
   *
   * @param span the source position of this token
   */
  case If(span: Span)

  /**
   * The `then` symbol.
   *
   * @param span the source position of this token
   */
  case Then(span: Span)

  /**
   * The `else` symbol.
   *
   * @param span the source position of this token
   */
  case Else(span: Span)

  /**
   * The `for` symbol.
   *
   * @param span the source position of this token
   */
  case For(span: Span)

  /**
   * The `while` symbol.
   *
   * @param span the source position of this token
   */
  case While(span: Span)

  /**
   * The `do` symbol.
   *
   * @param span the source position of this token
   */
  case Do(span: Span)

  /**
   * The `in` symbol.
   *
   * @param span the source position of this token
   */
  case In(span: Span)

  /**
   * The `def` symbol.
   *
   * @param span the source position of this token
   */
  case Def(span: Span)

  /**
   * The `val` symbol.
   *
   * @param span the source position of this token
   */
  case Val(span: Span)

  /**
   * The `mut` symbol.
   *
   * @param span the source position of this token
   */
  case Mut(span: Span)

  /**
   * The `package` symbol.
   *
   * @param span the source position of this token
   */
  case Package(span: Span)

  /**
   * The source position of this token.
   */
  def span: Span
