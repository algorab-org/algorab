package org.algorab.parser

import kyo.*
import org.algorab.ast.Identifier
import org.algorab.parser.debug

//Code source (String) -> Chunk(tokenA, tokenB, ...)
object Lexer:

  case class Block(indent: Int, layoutEnd: Maybe[Token], skipIndent: Boolean)

  case class State(lineStart: Int, blocks: Chunk[Block]):

    def currentBlock: Block = blocks.headMaybe.getOrElse(Block(0, Absent, false))

    def currentBlockIndent: Int = currentBlock.indent

    def doesCurrentBlockNeedIndent: Boolean = currentBlockIndent == -1

    def withCurrentBlockIndent(indent: Int): State =
      this.copy(blocks = blocks.head.copy(indent = indent) +: blocks.tail)

    def newline: State < Parse[Char] = Parse.position.map(pos => this.copy(lineStart = pos))

    def newBlock(layoutEnd: Maybe[Token]): State = this.copy(
      blocks = Block(-1, layoutEnd, false) +: blocks
    )

    def newParenthesizedBlock(layoutEnd: Token): State = this.copy(
      blocks = Block(-1, Present(layoutEnd), true) +: blocks
    )

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

    def pop1: State =
      this.copy(blocks = if blocks.isEmpty then blocks else blocks.tail)

    def pop(endToken: Token): (Int, State) =
      val dropped = blocks.dropWhile(_.layoutEnd.forall(_ != endToken))
      val droppedTail =
        if dropped.isEmpty then dropped
        else dropped.tail

      (blocks.size - droppedTail.size, this.copy(blocks = droppedTail))

    def column: Int < Parse[Char] = Parse.position.map(_ - lineStart)

  val parseString: Token < Parse[Char] =
    Parse.literal('\"')
      .andThen(
        Parse.repeatUntil(
          Parse.require(Parse.any),
          Parse.peek(Parse.not('\\')).andThen(Parse.literal('\"'))
        )
      )
      .map(chunk => Token.LString(chunk.mkString.translateEscapes))

  val parseTerm: Token < Parse[Char] = Parse.firstOf(
    Parse.boolean.map(Token.LBool.apply),
    Parse.firstOf(
      Parse.inOrder(
        Parse.decimal,
        Parse.anyIn("eE"),
        Parse.int
      ).map((mantissa, _, exponent) => Token.LFloat(mantissa * math.pow(10, exponent))),
      Parse.decimal.map(Token.LFloat.apply)
    ),
    Parse.int.map(Token.LInt.apply),
    parseString,
    Parse.identifier.map(str => Token.Ident(Identifier.assume(str.toString)))
  )

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
    "mut" -> Token.Mut
  )

  val blockStart: Map[Token, Maybe[Token]] = Map(
    Token.Equal -> Absent,
    // Token.Colon, TODO fix this in order to support multiline 0-arity HOF
    Token.In -> Present(Token.Do),
    Token.While -> Present(Token.Do),
    Token.Do -> Absent,
    Token.If -> Present(Token.Then),
    Token.Then -> Present(Token.Else),
    Token.Else -> Absent
  )

  val blockEnd: Chunk[Token] = Chunk.from(blockStart.values.flatten)

  val parentheses: Map[Token, Token] = Map(
    Token.ParenOpen -> Token.ParenClosed,
    Token.SquareOpen -> Token.SquareClosed
  )

  val parseSymbol: Token < Parse[Char] =
    Parse.firstOf(
      symbols
        .toList
        .sortBy((k, v) => -k.length) // Longer symbols first (e.g. >= before >)
        .map((k, v) => () => Parse.literal(k).andThen(v))
    )

  val parseKeyword: Token < Parse[Char] =
    Parse.identifier.map(kw =>
      keywords.get(kw.toString) match
        case Some(token) => token
        case None        => Parse.fail("Invalid keyword")
    )

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

  val skipAnyToken: Unit < Parse[Char] = Parse.firstOf(
    parseKeyword,
    parseTerm,
    parseSymbol
  ).unit

  // def parseIndent(level: Int): (Chunk[Token], Int) < Parse[Char] =
  //   Parse.repeat(Parse.literal("  ")).map(tabs =>
  //     val tabsSize = tabs.size
  //     val comparison = tabsSize - level
  //     if comparison == 0 then (Chunk.empty, tabsSize)
  //     else if comparison < 0 then (Chunk.fill(-comparison)(Chunk(Token.DeIndent, Token.Newline)).flattenChunk, tabsSize)
  //     else if comparison == 1 then (Chunk(Token.Indent), tabsSize)
  //     else Parse.fail("Too much indentation")
  //   )

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

  // def lexingIteration(state: State): (Chunk[Token], State) < Parse[Char] =
  //   Parse.firstOf(
  //     parseLineBreak(state).map((Chunk.empty, _)),
  //     parseAnyToken.map(token =>
  //       if blockStart.contains(token) then

  //       else (Chunk(token), state)
  //     )
  //   )

  // TODO only place indents when line terminates with `=`, `then` or `do`

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
