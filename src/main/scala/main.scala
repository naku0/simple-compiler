import lexer.Lexer

@main
def main(): Unit = {
  val codeExample = "def lol() = { } \n var x = 123 \n print x + 5;"
  val codeExample2 =
    """
    def main() = {
      print("Hello!")
    }
    """
  val tokens = Lexer.tokenize(codeExample2)
  tokens.foreach(println)
}
