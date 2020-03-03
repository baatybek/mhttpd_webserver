
sealed abstract class Token
case class Word(input: String) extends Token
object Word { val pattern = "^[a-zA-Z_][a-zA-Z_\\-0-9]*$".r }

case class Quoted(input: String) extends Token
object Quoted { val pattern = "^\"[^\n]*\"$".r }

case class OpenBrace() extends Token
object OpenBrace { val pattern = "^\\{$".r }

case class CloseBrace() extends Token
object CloseBrace { val pattern = "^}$".r }

case class OpenSquareBrace () extends Token
object OpenSquareBrace { val pattern = "^\\[$".r }

case class CloseSquareBrace () extends Token
object CloseSquareBrace { val pattern = "^]$".r }

case class Comma () extends Token
object Comma { val pattern = "^,$".r }

case class Column () extends Token
object Column { val pattern = "^:$".r }

case class NewLine() extends Token
object NewLine { val pattern = "^\n$".r  }

// We won't have white space tokens
case class Whitespace () extends  Token
object Whitespace { val pattern = "^[ \t]+$".r }

case class Stuff(input: String) extends Token
object Stuff { val pattern = "^[^\n\t]+$".r }

object Tokenizer { // How do you eat an elephant?

  def toTokens(input: String): List[Token] = {
    def recognizeToken(token: String): List[Token] = {
      token match {
        case Whitespace.pattern(_*) => Nil
        case Quoted.pattern(_*) => List(Quoted(token.slice(1, token.length - 1)))
        case OpenBrace.pattern(_*) => List(OpenBrace())
        case CloseBrace.pattern(_*) => List(CloseBrace())
        case OpenSquareBrace.pattern(_*) => List(OpenSquareBrace())
        case CloseSquareBrace.pattern(_*) => List(CloseSquareBrace())
        case Comma.pattern(_*) => List(Comma())
        case Column.pattern(_*) => List(Column())
        case NewLine.pattern(_*) => List(NewLine())
        case Word.pattern(_*) => List(Word(token))
        case Stuff.pattern(_*) => List(Stuff(token))
        case "" => List() // The first token might just be an empty token
        case _ => throw new IllegalStateException("Cannot create a token from this string: \"" + token + "\"")
      }
    }

    def tokenizeWhitespace(input: List[Char], candidate: String, accumulator: List[Token]): List[Token] = {
      input match {
        case Nil => accumulator ++  recognizeToken(candidate)
        case ' ' :: tail => tokenizeWhitespace(tail, candidate + ' ', accumulator)
        case '\t' :: tail => tokenizeWhitespace(tail, candidate + '\t', accumulator)
        case _ => tokenizeTopLevel(input, candidate = "", accumulator ++ recognizeToken(candidate))
      }
    }

    def tokenizeQuote(input: List[Char], candidate: String, accumulator: List[Token]): List[Token] = {
      input match {
        case Nil => accumulator ++ recognizeToken(candidate)
        case '\"' :: tail => tokenizeTopLevel(tail, "", accumulator ++ recognizeToken(candidate + '\"'))
        case '\n' :: _ => throw new IllegalStateException("New lines are not allowed in quoted tokens:" + candidate)
        case s :: tail => tokenizeQuote(tail, candidate + s, accumulator)
      }
    }

    def tokenizeTopLevel(input: List[Char], candidate: String, accumulator: List[Token]): List[Token] = {
      input match {
        case Nil => accumulator ++ recognizeToken(candidate)
        case ' ' :: tail => tokenizeWhitespace(tail, " ", accumulator ++ recognizeToken(candidate))
        case '\t' :: tail => tokenizeWhitespace(tail, "\t", accumulator ++ recognizeToken(candidate))
        case '\n' :: tail => tokenizeTopLevel(tail, "", accumulator ++ recognizeToken(candidate) ++ recognizeToken("\n"))
        case '\"' :: tail => tokenizeQuote(tail, "\"", recognizeToken(candidate) ++ accumulator)
        case letter :: tail => tokenizeTopLevel(tail, candidate + letter, accumulator)
      }
    }

    tokenizeTopLevel(input.toList, "", Nil)
  }
}

sealed abstract class ConfigOption
case class Param(name: String, value: String) extends ConfigOption
case class Context(path: String, handlerName: String) extends ConfigOption
case class HandlerSpec(name: String, clazz: Class[ContextHandler], options: List[Param]) extends ConfigOption

object Parser {
  def parse(input: List[Token]) = {
    def parseOptions(input: List[Token], output: List[Param]): (List[Token], List[Param]) = {
      input match {
        case Word(keyword) :: Word(name) :: Quoted(value) :: NewLine() :: tail
          if keyword.toLowerCase == "param" =>
          parseOptions(tail, Param(name, value) :: output)
        case Word(keyword) :: Word(name) :: Stuff(value) :: NewLine() :: tail
          if keyword.toLowerCase == "param" =>
          parseOptions(tail, Param(name, value) :: output )
        case Word(keyword) :: Word(name) :: Word(value) :: NewLine() :: tail
          if keyword.toLowerCase == "param" =>
          parseOptions(tail, Param(name, value) :: output )
        case CloseBrace() :: NewLine() :: tail =>
          (tail, output)
        case CloseBrace() :: Nil =>
          (Nil, output)
        case NewLine() :: tail =>
          parseOptions(tail, output)
        case CloseBrace() :: _ =>
          throw new IllegalStateException("No new line after closing brace")
        case Word(word) :: _ =>
          throw new IllegalStateException("Unknown token in handler param list: " + word)
        case Quoted(word) :: _ =>
          throw new IllegalStateException("Unknown quoted token in handler param list: " + word)
        case OpenBrace() :: _ =>
          throw new IllegalStateException("Illegal opening brace in handler param list")
        case Nil =>
          throw new IllegalStateException("No closing brace found for param list")
        case _ =>
          throw new IllegalStateException("Unknown parser error")
      }
    }

    def parseTopLevel(input: List[Token], output: List[ConfigOption]): List[ConfigOption] =
      input match {
        case Nil => output

        case Word(keyword) :: Word(name) :: Quoted(value) :: NewLine() :: tail
          if keyword.toLowerCase == "param" =>
          parseTopLevel(tail, Param(name, value) :: output)

        case Word(keyword) :: Word(name) :: Stuff(value) :: NewLine() :: tail
          if keyword.toLowerCase == "param" =>
          parseTopLevel(tail, Param(name, value) :: output)

        case Word(keyword) :: Word(path) :: Word(handler) :: NewLine() :: tail
          if keyword.toLowerCase == "context" =>
          parseTopLevel(tail, Context(path, handler) :: output)

        case Word(keyword) :: Stuff(path) :: Word(handler) :: NewLine() :: tail
          if keyword.toLowerCase == "context" =>
          parseTopLevel(tail, Context(path, handler) :: output)

        case Word(keyword) :: Word(name) :: Word(handler) :: OpenBrace() :: tail
          if keyword.toLowerCase == "handler" => {
          val handlerClass = Class.forName(handler).asInstanceOf[Class[ContextHandler]]
          val (newTail, params) = parseOptions(tail, Nil)
          parseTopLevel(newTail, HandlerSpec(name, handlerClass, params) :: output)
        }

        case Word(keyword) :: Word(name) :: Stuff(handler) :: NewLine() :: tail
          if keyword.toLowerCase == "handler" => {
          val handlerClass = Class.forName(handler).asInstanceOf[Class[ContextHandler]]
          parseTopLevel(tail, HandlerSpec(name, handlerClass, Nil) :: output)
        }

        case Word(param) :: Stuff (value) :: NewLine() :: tail =>
          parseTopLevel(tail, Param(param, value) :: output)

        case Word(param) :: Word (value) :: NewLine() :: tail =>
          parseTopLevel(tail, Param(param, value) :: output)

        case NewLine() :: tail => parseTopLevel(tail, output)


        case _ =>  throw new IllegalStateException("Cannot parse input: " + input + "\noutput so far: " + output)
      }

    parseTopLevel(input, Nil)
  }
}