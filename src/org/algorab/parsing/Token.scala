package org.algorab.parsing

import org.algorab.ast.Identifier
import org.algorab.util.SourcePosition

/**
 * A token, a "word" in the source code.
 */
enum Token derives CanEqual:

  /**
   * A boolean literal.
   *
   * @param value the literal value
   * @param position the source position of this token
   */
  case LBool(value: Boolean, position: SourcePosition)

  /**
   * An integer literal.
   *
   * @param value the literal value
   * @param position the source position of this token
   */
  case LInt(value: Int, position: SourcePosition)

  /**
   * A float literal.
   *
   * @param value the literal value
   * @param position the source position of this token
   */
  case LFloat(value: Double, position: SourcePosition)

  /**
   * A character literal.
   *
   * @param value the literal value
   * @param position the source position of this token
   */
  case LChar(value: Char, position: SourcePosition)

  /**
   * A string literal.
   *
   * @param value the literal value
   * @param position the source position of this token
   */
  case LString(value: String, position: SourcePosition)

  /**
   * An identifier.
   *
   * @param identifier the referenced name
   * @param position the source position of this token
   */
  case Ident(identifier: Identifier, position: SourcePosition)

  /**
   * An indentation token, similar to `{` in brace-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param position the source position of this token
   */
  case Indent(position: SourcePosition)

  /**
   * An de-indentation token, similar to `}` in brace-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param position the source position of this token
   */
  case DeIndent(position: SourcePosition)

  /**
   * An newline token, similar to `;` in semicolon-based languages.
   * This token is inserted by the lexer after parsing the other tokens.
   *
   * @param position the source position of this token
   */
  case Newline(position: SourcePosition)

  // Symbols

  /**
   * The `(` symbol.
   *
   * @param position the source position of this token
   */
  case ParenOpen(position: SourcePosition)

  /**
   * The `)` symbol.
   *
   * @param position the source position of this token
   */
  case ParenClosed(position: SourcePosition)

  /**
   * The `,` symbol.
   *
   * @param position the source position of this token
   */
  case Comma(position: SourcePosition)

  /**
   * The `:` symbol.
   *
   * @param position the source position of this token
   */
  case Colon(position: SourcePosition)

  /**
   * The `.` symbol.
   *
   * @param position the source position of this token
   */
  case Dot(position: SourcePosition)

  /**
   * The `+` symbol.
   *
   * @param position the source position of this token
   */
  case Plus(position: SourcePosition)

  /**
   * The `-` symbol.
   *
   * @param position the source position of this token
   */
  case Minus(position: SourcePosition)

  /**
   * The `*` symbol.
   *
   * @param position the source position of this token
   */
  case Mul(position: SourcePosition)

  /**
   * The `/` symbol.
   *
   * @param position the source position of this token
   */
  case Div(position: SourcePosition)

  /**
   * The `//` symbol.
   *
   * @param position the source position of this token
   */
  case IntDiv(position: SourcePosition)

  /**
   * The `%` symbol.
   *
   * @param position the source position of this token
   */
  case Percent(position: SourcePosition)

  /**
   * The `=` symbol.
   *
   * @param position the source position of this token
   */
  case Equal(position: SourcePosition)

  /**
   * The `==` symbol.
   *
   * @param position the source position of this token
   */
  case EqualEqual(position: SourcePosition)

  /**
   * The `!=` symbol.
   *
   * @param position the source position of this token
   */
  case NotEqual(position: SourcePosition)

  /**
   * The `<` symbol.
   *
   * @param position the source position of this token
   */
  case Less(position: SourcePosition)

  /**
   * The `<=` symbol.
   *
   * @param position the source position of this token
   */
  case LessEqual(position: SourcePosition)

  /**
   * The `>` symbol.
   *
   * @param position the source position of this token
   */
  case Greater(position: SourcePosition)

  /**
   * The `>=` symbol.
   *
   * @param position the source position of this token
   */
  case GreaterEqual(position: SourcePosition)

  // Keywords

  /**
   * The `and` symbol.
   *
   * @param position the source position of this token
   */
  case And(position: SourcePosition)

  /**
   * The `or` symbol.
   *
   * @param position the source position of this token
   */
  case Or(position: SourcePosition)

  /**
   * The `not` symbol.
   *
   * @param position the source position of this token
   */
  case Not(position: SourcePosition)

  /**
   * The `if` symbol.
   *
   * @param position the source position of this token
   */
  case If(position: SourcePosition)

  /**
   * The `then` symbol.
   *
   * @param position the source position of this token
   */
  case Then(position: SourcePosition)

  /**
   * The `else` symbol.
   *
   * @param position the source position of this token
   */
  case Else(position: SourcePosition)

  /**
   * The `for` symbol.
   *
   * @param position the source position of this token
   */
  case For(position: SourcePosition)

  /**
   * The `while` symbol.
   *
   * @param position the source position of this token
   */
  case While(position: SourcePosition)

  /**
   * The `do` symbol.
   *
   * @param position the source position of this token
   */
  case Do(position: SourcePosition)

  /**
   * The `in` symbol.
   *
   * @param position the source position of this token
   */
  case In(position: SourcePosition)

  /**
   * The `def` symbol.
   *
   * @param position the source position of this token
   */
  case Def(position: SourcePosition)

  /**
   * The `val` symbol.
   *
   * @param position the source position of this token
   */
  case Val(position: SourcePosition)

  /**
   * The `mut` symbol.
   *
   * @param position the source position of this token
   */
  case Mut(position: SourcePosition)

  /**
   * The `package` symbol.
   *
   * @param position the source position of this token
   */
  case Package(position: SourcePosition)

  /**
   * The `import` symbol.
   *
   * @param position the source position of this token
   */
  case Import(position: SourcePosition)

  /**
   * The `as` symbol.
   *
   * @param position the source position of this token
   */
  case As(position: SourcePosition)

  /**
    * TODO
    *
    * @param position
    */
  case Invalid(position: SourcePosition)

  case Unknown(source: String, position: SourcePosition)

  /**
   * The source position of this token.
   */
  def position: SourcePosition

object Token:

  type ToString[A <: Token] = A match
    case Token.LBool        => "true or false"
    case Token.LInt         => "Int literal"
    case Token.LFloat       => "Float literal"
    case Token.LChar        => "Char literal"
    case Token.LString      => "String literal"
    case Token.Ident        => "identifier"
    case Token.Indent       => "indentation"
    case Token.DeIndent     => "de-indentation"
    case Token.Newline      => "new line"
    case Token.ParenOpen    => "("
    case Token.ParenClosed  => ")"
    case Token.Comma        => ","
    case Token.Colon        => ":"
    case Token.Dot          => "."
    case Token.Plus         => "+"
    case Token.Minus        => "-"
    case Token.Mul          => "*"
    case Token.Div          => "/"
    case Token.IntDiv       => "//"
    case Token.Percent      => "%"
    case Token.Equal        => "="
    case Token.EqualEqual   => "=="
    case Token.NotEqual     => "!="
    case Token.Less         => "<"
    case Token.LessEqual    => "<="
    case Token.Greater      => ">"
    case Token.GreaterEqual => ">="
    case Token.And          => "and"
    case Token.Or           => "or"
    case Token.Not          => "not"
    case Token.If           => "if"
    case Token.Then         => "then"
    case Token.Else         => "else"
    case Token.For          => "for"
    case Token.While        => "while"
    case Token.Do           => "do"
    case Token.In           => "in"
    case Token.Def          => "def"
    case Token.Val          => "val"
    case Token.Mut          => "mut"
    case Token.Package      => "package"
    case Token.Import       => "import"
    case Token.As           => "as"
    case Token.Invalid      => "<invalid>"
