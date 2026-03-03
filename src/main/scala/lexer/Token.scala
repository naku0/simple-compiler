package lexer

case class Token(tokenType: TokenType,
                 value: String,
                 position: Int,
                 line: Int,
                 column: Int) {
  override def toString: String = s"[$line:$column] Token($tokenType, '$value')"
}
