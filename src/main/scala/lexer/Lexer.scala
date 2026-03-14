package lexer

import lexer.maps.{Keywords, Operators}

import scala.annotation.tailrec

case class Lexer(source: String,
                 position: Int = 0,
                 line: Int = 1,
                 column: Int = 1) {
  def isAtEnd: Boolean = position >= source.length
  def currentChar: Char = if isAtEnd then '\u0000' else source(position)
  def peekNext: Char = if position + 1 < source.length then source(position + 1) else '\u0000'
}

object Lexer {
  def tokenize(input: String): List[Token] = {
    val source = Option(input).getOrElse("")
    tokenize(Lexer(source)).toList
  }

  @tailrec
  private def tokenize(state: Lexer, acc: List[Token] = Nil): LazyList[Token] = {
    if state.isAtEnd then
      LazyList.from((eofToken(state) :: acc).reverse)
    else
      val (newState, maybeToken) = nextToken(state)
      maybeToken match
        case Some(token) => tokenize(newState, token :: acc)
        case None => tokenize(newState, acc)
  }

  private def nextToken(state: Lexer): (Lexer, Option[Token]) = state.currentChar match {
    case c if c.isWhitespace => (skipWhitespace(state), None)
    case c if c.isDigit => readNumber(state)
    case c if c.isLetter => readWord(state)
    case '"' => readString(state)
    case _ => readOperatorOrPunctuation(state)
  }

  private def readString(state: Lexer): (Lexer, Option[Token]) = {
    val start = state
    val afterOpen = advance(state)

    @tailrec
    def loop(currentState: Lexer, chars: List[Char] = Nil): (Lexer, List[Char]) = {
      if currentState.isAtEnd then
        throw new Exception(s"[Lexer Error] Unterminated string at Line ${start.line}, Column ${start.column}")
      else if currentState.currentChar == '"' then
        (currentState, chars.reverse)
      else
        loop(advance(currentState), currentState.currentChar :: chars)
    }

    val (stateAtCloseQuote, charList) = loop(afterOpen)
    val content = charList.mkString
    val newState = advance(stateAtCloseQuote)

    (newState, Some(Token(TokenType.STRING, content, start.position, start.line, start.column)))
  }

  private def readNumber(state: Lexer): (Lexer, Option[Token]) = {
    val start = state

    @tailrec
    def loop(currentState: Lexer): Lexer =
      if !currentState.isAtEnd && currentState.currentChar.isDigit then
        loop(advance(currentState))
      else
        currentState

    val newState = loop(state)
    val text = state.source.substring(start.position, newState.position)
    (newState, Some(Token(TokenType.NUMBER, text, start.position, start.line, start.column)))
  }

  private def readWord(state: Lexer): (Lexer, Option[Token]) = {
    val start = state

    @tailrec
    def loop(currentState: Lexer): Lexer =
      if !currentState.isAtEnd && currentState.currentChar.isLetterOrDigit then
        loop(advance(currentState))
      else
        currentState

    val newState = loop(state)
    val text = state.source.substring(start.position, newState.position)
    val tokenType = Keywords.keywords.getOrElse(text, TokenType.ID)
    (newState, Some(Token(tokenType, text, start.position, start.line, start.column)))
  }

  private def readOperatorOrPunctuation(state: Lexer): (Lexer, Option[Token]) = {
    val start = state

    val twoCharsOp = if !state.isAtEnd && !state.peekNext.isWhitespace then
      Operators.getTokenType(s"${state.currentChar}${state.peekNext}")
        .map(tokenType => (s"${state.currentChar}${state.peekNext}", tokenType))
    else None

    twoCharsOp match {
      case Some((opStr, tokenType)) =>
        val newState = advance(advance(state))
        (newState, Some(Token(tokenType, opStr, start.position, start.line, start.column)))

      case None =>
        val oneChar = state.currentChar.toString
        Operators.getTokenType(oneChar) match {
          case Some(tokenType) =>
            (advance(state), Some(Token(tokenType, oneChar, start.position, start.line, start.column)))
          case None =>
            throw new Exception(s"[Lexer Error] Unexpected character '$oneChar' at Line ${start.line}, Column ${start.column}")
        }
    }
  }

  private def skipWhitespace(state: Lexer): Lexer = {
    @tailrec
    def loop(currentState: Lexer): Lexer =
      if !currentState.isAtEnd && currentState.currentChar.isWhitespace then
        loop(advance(currentState))
      else
        currentState

    loop(state)
  }

  private def advance(state: Lexer): Lexer = {
    if state.isAtEnd then state
    else if state.currentChar == '\n' then
      state.copy(
        position = state.position + 1,
        line = state.line + 1,
        column = 1
      )
    else
      state.copy(
        position = state.position + 1,
        column = state.column + 1
      )
  }

  private def eofToken(state: Lexer): Token =
    Token(TokenType.EOF, "\u0000", state.position, state.line, state.column)
}
