import scala.io.Source
import scala.util.{Try, Using}

object Reader:
  def readFromFile(fileName: String): String = {
    Try {
      Using.resource(Source.fromFile(fileName)) { source =>
        source.getLines().mkString
      }
    } match {
      case scala.util.Success(text) if text.nonEmpty => text
      case scala.util.Success(_) => throw new RuntimeException("Файл пуст")
      case scala.util.Failure(exception) => throw new RuntimeException(s"Ошибка чтения файла: ${exception.getMessage}")
    }
  }