/** Algorab lexer: converts raw source text into a flat token stream.
  *
  * The [[Lexer]] object implements indentation-sensitive layout rules similar to those of
  * Haskell or Python.  Indented blocks are delimited by synthetic [[Token.Indent]] /
  * [[Token.DeIndent]] tokens rather than explicit braces, and statements are separated by
  * synthetic [[Token.Newline]] tokens rather than semicolons.
  *
  * The main entry point is [[Lexer.parseTokens]], which consumes an entire source string and
  * produces a `Chunk[Token]`.
  */
package org.algorab.parser

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.parser.debug

/** Converts a stream of characters into a stream of [[Token]]s.
  *
  * Layout processing is handled by a stack of [[Lexer.Block]] entries maintained inside
  * [[Lexer.State]].  A new block is pushed whenever the lexer encounters a token that
  * opens a layout context (e.g. `=`, `then`, `do`), and popped when the block's
  * optional terminator token is encountered or when dedentation indicates the block has ended.
  */
object Lexer:

  /** Represents an open layout block on the block stack.
    *
    * @param indent      the column position that all statements in this block must start at;
    *                    `-1` means the indent has not yet been determined (set on the first token)
    * @param layoutEnd   the optional token that explicitly closes this block (e.g. `Token.Do` for
    *                    `if … then`, `Token.Else` for `then …`)
    * @param skipIndent  `true` for parenthesised blocks (`(…)`, `[…]`) where indentation is ignored
    */
  case class Block(indent: Int, layoutEnd: Maybe[Token], skipIndent: Boolean)

  /** The mutable layout state threaded through the lexer.
    *
    * @param lineStart the character position of the first character on the current line;
    *                  used to compute the current column via `Parse.position - lineStart`
    * @param blocks    the current stack of open layout blocks, innermost first
    */
  case class State(lineStart: Int, blocks: Chunk[Block]):

    /** Returns the innermost open block, or a default root block if the stack is empty. */
    def currentBlock: Block = blocks.headMaybe.getOrElse(Block(0, Absent, false))

    /** Returns the indentation level of the innermost open block. */
    def currentBlockIndent: Int = currentBlock.indent

    /** `true` when the innermost block's indentation has not yet been determined. */
    def doesCurrentBlockNeedIndent: Boolean = currentBlockIndent == -1

    /** Returns a copy of this state with the innermost block's indentation set to `indent`. */
    def withCurrentBlockIndent(indent: Int): State =
      this.copy(blocks = blocks.head.copy(indent = indent) +: blocks.tail)

    /** Records a new line start by resetting `lineStart` to the current parse position.
      *
      * @return an updated [[State]] with `lineStart` set to the current position
      */
    def newline: State < Parse[Char] = Parse.position.map(pos => this.copy(lineStart = pos))

    /** Pushes a new layout block onto the stack.
      *
      * The new block's indent is initialised to `-1` (not yet determined) and will be
      * set to the column of the first token found inside the block.
      *
      * @param layoutEnd the token that terminates this block, if any
      * @return the updated [[State]]
      */
    def newBlock(layoutEnd: Maybe[Token]): State = this.copy(
      blocks = Block(-1, layoutEnd, false) +: blocks
    )

    /** Pushes a new ''parenthesised'' block that ignores indentation rules.
      *
      * @param layoutEnd the closing-bracket token that terminates the block
      * @return the updated [[State]]
      */
    def newParenthesizedBlock(layoutEnd: Token): State = this.copy(
      blocks = Block(-1, Present(layoutEnd), true) +: blocks
    )

    /** Pops blocks from the stack until reaching one whose indentation matches `indent`,
      * emitting a [[Token.DeIndent]] for each popped block.
      *
      * Fails with a parse error if no block with exactly `indent` indentation exists
      * (indicating inconsistent indentation in the source).
      *
      * @param indent the column position to dedent to
      * @return a pair of `(deIndentTokens, updatedState)`, effectful in `Parse[Char]`
      */
    def popUntil(indent: Int): (Chunk[Token], State) < Parse[Char] =
      val remainingIndents = blocks.dropWhile(block => !block.skipIndent && indent < block.indent)
      val newCurrentBlock = remainingIndents.headMaybe.getOrElse(Block(0, Absent, false))
      if newCurrentBlock.indent == indent || newCurrentBlock.skipIndent then
        (
          Chunk.fill(blocks.size - remainingIndents.size)(Token.DeIndent),
          this.copy(blocks = remainingIndents)
        )
      else
        Parse.fail(s"Inconsistent indentation: $indent spaces (or tabs) instead of ${newCurrentBlock.indent}")

    /** Pops exactly the innermost block without emitting tokens. */
    def pop1: State =
      this.copy(blocks = if blocks.isEmpty then blocks else blocks.tail)

    /** Pops all blocks up to and including the one whose `layoutEnd` matches `endToken`.
      *
      * @param endToken the closing token that triggers the pop
      * @return `(numberOfPoppedBlocks, updatedState)`
      */
    def pop(endToken: Token): (Int, State) =
      val dropped = blocks.dropWhile(_.layoutEnd.forall(_ != endToken))
      val droppedTail =
        if dropped.isEmpty then dropped
        else dropped.tail

      (blocks.size - droppedTail.size, this.copy(blocks = droppedTail))

    /** Computes the column of the current parse position relative to `lineStart`. */
    def column: Int < Parse[Char] = Parse.position.map(_ - lineStart)

  // ── Primitive parsers ──────────────────────────────────────────────────────

  /** Parses a decimal integer from the current position.
    *
    * Consumes as many digit characters as available and converts them to an `Int`.
    * Fails if no digits are present or the resulting value overflows `Int`.
    */
  val parseInt: Int < Parse[Char] =
    Parse.read: in =>
      val num = in.remaining.takeWhile(_.isDigit)
      Maybe
        .fromOption(num.mkString.toIntOption)
        .toResult(Result.fail(Chunk(ParseFailure("Invalid int", in.position))))
        .map(res => (in.advance(num.length), res))

  /** Parses the exponent part of a scientific-notation float (e.g. `+3`, `-12`, `7`).
    *
    * Consumes digits and optional leading sign, converts to `Int`.
    */
  val parseExponent: Int < Parse[Char] =
    Parse.read: in =>
      val num = in.remaining.takeWhile(c => c.isDigit || c == '-' || c == '+')
      Maybe
        .fromOption(num.mkString.toIntOption)
        .toResult(Result.fail(Chunk(ParseFailure("Invalid exponent", in.position))))
        .map(res => (in.advance(num.length), res))

  /** Parses a decimal floating-point literal of the form `digits.digits`.
    *
    * Delegates to `Parse.regex` and then converts the matched string to a `Double`.
    */
  val parseDecimal: Double < Parse[Char] =
    Parse.regex(raw"[0-9]+\.[0-9]+").map(str =>
      str.toString.toDoubleOption match
        case None        => Parse.fail("Invalid float literal")
        case Some(value) => value
    )

  /** Parses a double-quoted string literal, including escape sequences.
    *
    * The content between the quotes is collected character by character (stopping on
    * an unescaped `"`), then processed through `String.translateEscapes`.
    */
  val parseString: Token < Parse[Char] =
    Parse.spaced(
      Parse.literal('\"')
        .andThen(
          Parse.repeatUntil(
            Parse.require(Parse.any),
            Parse.peek(Parse.not('\\')).andThen(Parse.literal('\"'))
          )
        ),
      isWhitespace = _ => false,
      overrideOuter = true
    )
      .map(chunk => Token.LString(chunk.mkString.translateEscapes))

  /** Parses a single lexical term token (literal, identifier, boolean, or float/int).
    *
    * Tries alternatives in this order:
    *   1. Boolean literals (`true` / `false`).
    *   1. Floating-point with exponent (e.g. `1.5e3`).
    *   1. Plain decimal (e.g. `3.14`).
    *   1. Integer (e.g. `42`).
    *   1. String literal.
    *   1. Identifier.
    */
  val parseTerm: Token < Parse[Char] = Parse.firstOf(
    Parse.boolean.map(Token.LBool.apply),
    Parse.firstOf(
      Parse.inOrder(
        Parse.firstOf(parseDecimal, parseInt.map(_.toDouble)),
        Parse.anyIn("eE"),
        parseExponent
      ).map((mantissa, _, exponent) => Token.LFloat(mantissa * math.pow(10, exponent))),
      parseDecimal.map(Token.LFloat.apply)
    ),
    parseInt.map(Token.LInt.apply),
    parseString,
    Parse.identifier.map(str => Token.Ident(Identifier.assume(str.toString)))
  )

  /** Mapping from symbol strings to their corresponding [[Token]] values.
    *
    * Longer symbols are given priority during parsing so that, for example, `>=`
    * is matched before `>`.
    */
  val symbols: Map[String, Token] = Map(
    "(" -> Token.ParenOpen,
    ")" -> Token.ParenClosed,
    "[" -> Token.SquareOpen,
    "]" -> Token.SquareClosed,
    "{" -> Token.BraceOpen,
    "}" -> Token.BraceClosed,
    "." -> Token.Dot,
    "," -> Token.Comma,
    ":" -> Token.Colon,
    "+" -> Token.Plus,
    "-" -> Token.Minus,
    "*" -> Token.Mul,
    "/" -> Token.Div,
    "//" -> Token.IntDiv,
    "%" -> Token.Percent,
    "=>" -> Token.DoubleArrow,
    "=" -> Token.Equal,
    "==" -> Token.EqualEqual,
    "!=" -> Token.NotEqual,
    "<" -> Token.Less,
    "<=" -> Token.LessEqual,
    ">" -> Token.Greater,
    ">=" -> Token.GreaterEqual
  )

  /** Mapping from reserved word strings to their corresponding [[Token]] values. */
  val keywords: Map[String, Token] = Map(
    "and" -> Token.And,
    "or" -> Token.Or,
    "not" -> Token.Not,
    "if" -> Token.If,
    "then" -> Token.Then,
    "else" -> Token.Else,
    "for" -> Token.For,
    "while" -> Token.While,
    "do" -> Token.Do,
    "in" -> Token.In,
    "def" -> Token.Def,
    "val" -> Token.Val,
    "mut" -> Token.Mut,
    "class" -> Token.Class
  )

  /** Tokens that ''open'' a new layout block, mapped to the optional token that closes them.
    *
    * When the lexer encounters one of these tokens it pushes a new [[Block]] onto the stack
    * and emits an [[Token.Indent]] after the triggering token.  A `Present(end)` value means
    * the block is terminated by `end`; `Absent` means it is terminated only by dedentation.
    */
  val blockStart: Map[Token, Maybe[Token]] = Map(
    Token.Equal -> Absent,
    // Token.Colon, TODO fix this in order to support multiline 0-arity HOF
    Token.In -> Present(Token.Do),
    Token.While -> Present(Token.Do),
    Token.Do -> Absent,
    Token.If -> Present(Token.Then),
    Token.Then -> Present(Token.Else),
    Token.Else -> Absent,
    Token.Equal -> Absent
  )

  /** The set of tokens that can ''end'' a layout block (the values of [[blockStart]]). */
  val blockEnd: Chunk[Token] = Chunk.from(blockStart.values.flatten)

  /** Tokens that open a parenthesised (indentation-ignoring) block, mapped to their closers. */
  val parentheses: Map[Token, Token] = Map(
    Token.ParenOpen -> Token.ParenClosed,
    Token.SquareOpen -> Token.SquareClosed
  )

  /** Parses the longest matching symbol token from the current position.
    *
    * The symbol list is sorted by descending length so that multi-character operators
    * (e.g. `<=`) take precedence over single-character prefixes (e.g. `<`).
    */
  val parseSymbol: Token < Parse[Char] =
    Parse.firstOf(
      symbols
        .toList
        .sortBy((k, v) => -k.length) // Longer symbols first (e.g. >= before >)
        .map((k, v) => () => Parse.literal(k).andThen(v))
    )

  /** Parses a keyword token from the current position.
    *
    * Reads the next identifier-shaped word and looks it up in [[keywords]].
    * Fails if the word is not a recognised keyword.
    */
  val parseKeyword: Token < Parse[Char] =
    Parse.identifier.map(kw =>
      keywords.get(kw.toString) match
        case Some(token) => token
        case None        => Parse.fail("Invalid keyword")
    )

  /** Parses a line break (LF, CR+LF, or CR) and updates the line-start position.
    *
    * @param state the current lexer state, used to record the new line start
    * @return the updated [[State]] after advancing past the line break
    */
  def parseLineBreak(state: State): State < Parse[Char] =
    Parse.spaced(
      Parse.firstOf(
        Parse.literal("\r\n"),
        Parse.literal('\r'),
        Parse.literal('\n')
      ).andThen(state.newline),
      isWhitespace = _ => false,
      overrideOuter = true
    )

  /** Skips a single-line (`--`) or multi-line (`---…---`) comment.
    *
    * Single-line comments extend from `--` to the end of the line.
    * Multi-line comments are delimited by `---` markers and may span multiple lines.
    *
    * @param state the current lexer state (passed through line-break tracking)
    * @return the updated [[State]] after the comment
    */
  def discardComment(state: State): State < Parse[Char] = Parse.firstOf(
    Parse.inOrder(
      Parse.literal("---"),
      Parse.skipUntil(Parse.firstOf(
        Parse.literal("---").andThen(state),
        parseLineBreak(state).map(discardComment)
      ))
    ).map(_._2),
    Parse.inOrder(
      Parse.andIs(Parse.literal("--"), Parse.not(Parse.literal("---"))),
      Parse.skipUntil(Parse.firstOf(parseLineBreak(state), Parse.end.andThen(state)))
    ).map(_._2)
  )

  /** Parses the next token with error recovery.
    *
    * Attempts to parse a keyword, term, or symbol.  On failure the offending
    * character is skipped and the parser retries from the next character, continuing
    * until end of input.  This prevents a single bad character from aborting the
    * entire lexing pass.
    */
  val parseAnyToken: Token < Parse[Char] =
    val token =
      withErrorMessage(
        Parse.firstOf(
          parseKeyword,
          parseTerm,
          parseSymbol
        ),
        "Invalid token"
      )

    Parse.recoverWith(
      token,
      RecoverStrategy.skipThenRetryUntil(Parse.any, Parse.end)
    )

  /** Parses the next token without error recovery. Used internally for lookahead. */
  val skipAnyToken: Unit < Parse[Char] = Parse.firstOf(
    parseKeyword,
    parseTerm,
    parseSymbol
  ).unit

  /** Repeatedly applies `element(state)` until `until` succeeds, threading the [[State]]
    * through each iteration.
    *
    * Unlike `Parse.repeat`, this combinator passes the accumulated state to each call of
    * `element` so that layout bookkeeping is preserved across iterations.
    *
    * @param init    the initial [[State]]
    * @param element a function from the current state to `(output, nextState)`
    * @param until   a parser that signals the end of the repetition
    * @tparam Out    the type of each produced output value
    * @tparam State  the type of the threaded state
    * @return `(collectedOutputs, finalState)`
    */
  def repeatUntilState[Out, State](using
      Frame
  )[In, S](init: State)(
      element: State => (Out, State) < (Parse[In] & S),
      until: => Any < (Parse[In] & S)
  )(using Tag[Parse[In]]): (Chunk[Out], State) < (Parse[In] & S) =
    Parse.firstOf(
      until.andThen((Chunk.empty[Out], init)),
      for
        (head, state) <- element(init)
        (tail, newState) <- repeatUntilState(state)(element, until)
      yield (head +: tail, newState)
    )

  /** The top-level lexer parser.
    *
    * Consumes the entire input string and produces a `Chunk[Token]` with full layout
    * processing applied.  Specifically:
    *
    *   - Whitespace (spaces and tabs) is skipped between tokens.
    *   - Comments are discarded.
    *   - Line breaks update the line-start position for column tracking.
    *   - When the column of a token equals the current block indent, a [[Token.Newline]]
    *     is inserted before the token.
    *   - When the column is less than the current block indent, [[Token.DeIndent]]s are
    *     inserted to close all deeper blocks, followed by a [[Token.Newline]].
    *   - When a block-opening token is encountered (see [[blockStart]]), an [[Token.Indent]]
    *     is appended and a new block is pushed.
    *   - Opening parentheses / square brackets push a parenthesised block that suspends
    *     indentation rules until the matching closing bracket.
    *   - At end of input, outstanding open blocks are closed with [[Token.DeIndent]]s.
    */
  val parseTokens: Chunk[Token] < Parse[Char] =
    Parse.entireInput(
      Parse.spaced(
        repeatUntilState(State(0, Chunk.empty))(
          state =>
            Parse.firstOf(
              discardComment(state).map((Chunk.empty, _)),
              parseLineBreak(state).map((Chunk.empty, _)),
              Parse.require(
                Parse.inOrder(state.column, parseAnyToken).map((column, token) =>
                  val indentManagement: (Chunk[Token], State) < Parse[Char] =
                    if state.currentBlock.skipIndent then
                      if state.currentBlock.layoutEnd.exists(_ == token) then (Chunk(token), state.pop1)
                      else (Chunk(token), state)
                    else
                      val st =
                        if state.doesCurrentBlockNeedIndent then state.withCurrentBlockIndent(column)
                        else state

                      if blockEnd.contains(token) then
                        val (n, newState) = st.pop(token)
                        (Chunk.fill(n)(Token.DeIndent) :+ token, newState)
                      else if column == state.currentBlockIndent then (Chunk(Token.Newline, token), st)
                      else if column < st.currentBlockIndent then
                        st.popUntil(column).map((deindents, newState) =>
                          (deindents :+ Token.Newline :+ token, newState)
                        )
                      else (Chunk(token), st)

                  indentManagement.map((tokens2, st2) =>
                    if blockStart.contains(token) then (tokens2 ++ Chunk(Token.Indent), st2.newBlock(blockStart(token)))
                    else if parentheses.contains(token) then
                      (tokens2, st2.newParenthesizedBlock(parentheses(token)))
                    else (tokens2, st2)
                  )
                )
              )
            ),
          Parse.end
        )
          .map((tokens, state) => state.popUntil(0).map((deindents, _) => tokens.flattenChunk ++ deindents)),
        isWhitespace = c => c.isSpaceChar || c == '\t'
      )
    )
