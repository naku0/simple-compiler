import lexer.Lexer
import parser.Parser

@main
def main(): Unit = {
  val codeExample = "var x = 123;"
  val codeExample2 = Reader.readFromFile("data")

  val tokens = Lexer.tokenize(codeExample2)
  val parsedVals = Parser.parse(tokens)
  parsedVals.foreach(println)
  //tokens.foreach(println)
}
