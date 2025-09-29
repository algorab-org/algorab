package org.algorab.parser

import kyo.*
import org.algorab.ast.Identifier

//Code source (String) -> Chunk(tokenA, tokenB, ...)
object Lexer:

  def debug[A, In, S](name: String)(parser: A < (Parse[In] & S))(using Tag[In], Frame): A < (Parse[In] & S) =
    for
      start <- Parse.position
      _ = println(s"$name start $start")
      result <- parser
      endPos <- Parse.position
      _ = println(s"$name end $endPos: \"$result\"")
    yield result

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
    Parse.int.map(Token.LInt.apply),
    Parse.decimal.map(Token.LFloat.apply),
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

  val parseLineBreak: Token < Parse[Char] = Parse.firstOf(
    Parse.literal("\r\n"),
    Parse.literal('\r'),
    Parse.literal('\n')
  ).andThen(Token.Newline)

  val discardComment: Unit < Parse[Char] = Parse.firstOf(
    Parse.inOrder(
      Parse.literal("---"),
      Parse.skipUntil(Parse.literal("---"))
    ).unit,
    Parse.inOrder(
      Parse.andIs(Parse.literal("--"), Parse.not("---")),
      Parse.skipUntil(Parse.firstOf(parseLineBreak, Parse.end))
    ).unit
  )

  def parseAnyToken(level: Int): (Chunk[Token], Int) < Parse[Char] = Parse.firstOf(
    Parse.spaced(
      Parse.firstOf(
        parseKeyword,
        parseTerm,
        parseSymbol
      ),
      isWhitespace = _.isSpaceChar
    ).map(token => (Chunk(token), level)),
    Parse.inOrder(parseLineBreak, parseIndent(level))
      .map:
        case (br, (idents, newLevel)) => (br +: idents, newLevel),
    Parse.require(Parse.fail("Invalid Token"))
  )

  val skipAnyToken: Unit < Parse[Char] = Parse.firstOf(
    parseKeyword,
    parseTerm,
    parseSymbol
  ).unit

  def parseIndent(level: Int): (Chunk[Token], Int) < Parse[Char] =
    Parse.repeat(Parse.literal("  ")).map(tabs =>
      val tabsSize = tabs.size
      val comparison = tabsSize - level
      if comparison == 0 then (Chunk.empty, tabsSize)
      else if comparison < 0 then (Chunk.fill(-comparison)(Chunk(Token.DeIndent, Token.Newline)).flattenChunk, tabsSize)
      else if comparison == 1 then (Chunk(Token.Indent), tabsSize)
      else Parse.fail("Too much indentation")
    )

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

  val parseTokens: Chunk[Token] < Parse[Char] =
    Parse.entireInput(
      repeatUntilState(0)(
        level =>
          Parse.require(
            Parse.recoverWith(
              Parse.repeat(discardComment)
                .andThen(
                  parseAnyToken(level)
                ),
              RecoverStrategy.skipThenRetryUntil(Parse.any, skipAnyToken)
            )
          ),
        Parse.end
      ).map((tokenss, remainingLevel) => tokenss.flattenChunk ++ Chunk.fill(remainingLevel)(Token.DeIndent))
    )
