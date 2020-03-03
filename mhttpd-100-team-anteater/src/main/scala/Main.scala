import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source

object Main extends App {
  val contents: String = Source.fromFile("configuration.txt").mkString.replace("\r\n", "\n" )
  val tokens = Tokenizer.toTokens(contents)
  val options = Parser.parse(tokens)
  val config: Configuration = new OptionConfiguration(options)

  Server.runForever( config )
}
