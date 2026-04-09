/** Lexer token definitions for the Algorab language.
  *
  * Each [[Token]] variant corresponds to a lexical unit recognised by [[Lexer]].
  * Tokens are consumed by [[Parser]] to build the untyped AST.
  */
package org.algorab.parser

import org.algorab.ast.Identifier

/** A single lexical token produced by the [[Lexer]].
  *
  * Tokens are grouped into:
  *   - '''Literal tokens''' – carry the parsed value of the literal.
  *   - '''Layout tokens''' – `Indent`, `DeIndent`, `Newline` – produced by the
  *     indentation-sensitive layout algorithm; they delimit blocks without requiring
  *     explicit braces.
  *   - '''Symbol tokens''' – single or multi-character punctuation.
  *   - '''Keyword tokens''' – reserved words of the language.
  */
enum Token derives CanEqual:
  // ── Literals ──────────────────────────────────────────────────────────────

  /** A boolean literal (`true` or `false`). */
  case LBool(value: Boolean)

  /** An integer literal. */
  case LInt(value: Int)

  /** A floating-point literal (includes scientific-notation forms). */
  case LFloat(value: Double)

  /** A character literal (currently unused in the lexer but reserved). */
  case LChar(value: Char)

  /** A string literal with escape sequences already translated. */
  case LString(value: String)

  /** An identifier token (variable name, type name, etc.).
    *
    * @param identifier the validated non-blank identifier string
    */
  case Ident(identifier: Identifier)

  // ── Layout tokens ─────────────────────────────────────────────────────────

  /** Marks the start of an indented block (analogous to `{` in brace-based languages). */
  case Indent

  /** Marks the end of an indented block (analogous to `}` in brace-based languages). */
  case DeIndent

  /** Separates adjacent statements within a block (analogous to `;`). */
  case Newline

  // ── Symbol tokens ─────────────────────────────────────────────────────────

  /** `(` – opens a parenthesised expression or argument list. */
  case ParenOpen

  /** `)` – closes a parenthesised expression or argument list. */
  case ParenClosed

  /** `[` – opens a type-argument or array-index bracket. */
  case SquareOpen

  /** `]` – closes a type-argument or array-index bracket. */
  case SquareClosed

  /** `{` – opening brace (reserved; currently unused in the grammar). */
  case BraceOpen

  /** `}` – closing brace (reserved; currently unused in the grammar). */
  case BraceClosed

  /** `.` – member-selection dot. */
  case Dot

  /** `,` – argument / element separator. */
  case Comma

  /** `:` – type-annotation separator. */
  case Colon

  /** `+` – addition operator or unary plus. */
  case Plus

  /** `-` – subtraction operator or unary negation. */
  case Minus

  /** `*` – multiplication operator. */
  case Mul

  /** `/` – floating-point division operator. */
  case Div

  /** `//` – integer (truncating) division operator. */
  case IntDiv

  /** `%` – modulo operator. */
  case Percent

  /** `=>` – function-type arrow. */
  case DoubleArrow

  /** `=` – assignment or definition separator. */
  case Equal

  /** `==` – equality comparison operator. */
  case EqualEqual

  /** `!=` – inequality comparison operator. */
  case NotEqual

  /** `<` – less-than comparison operator. */
  case Less

  /** `<=` – less-than-or-equal comparison operator. */
  case LessEqual

  /** `>` – greater-than comparison operator. */
  case Greater

  /** `>=` – greater-than-or-equal comparison operator. */
  case GreaterEqual

  // ── Keyword tokens ────────────────────────────────────────────────────────

  /** `and` – short-circuit logical AND. */
  case And

  /** `or` – short-circuit logical OR. */
  case Or

  /** `not` – logical negation. */
  case Not

  /** `if` – start of a conditional expression. */
  case If

  /** `then` – separates the condition from the true branch. */
  case Then

  /** `else` – separates the true branch from the false branch. */
  case Else

  /** `for` – start of a `for` loop. */
  case For

  /** `while` – start of a `while` loop. */
  case While

  /** `do` – separates the loop condition/range from the loop body. */
  case Do

  /** `in` – separates the loop variable from the iterable in a `for` loop. */
  case In

  /** `def` – introduces a function definition. */
  case Def

  /** `val` – introduces a variable binding. */
  case Val

  /** `mut` – modifier that makes a `val` binding mutable. */
  case Mut

  /** `class` – introduces a class definition. */
  case Class
