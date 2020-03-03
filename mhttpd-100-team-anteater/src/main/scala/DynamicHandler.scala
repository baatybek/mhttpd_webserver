import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import NetworkService.{Request, RequestMethod, Response}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Data ( val name: String, val messageDateTime: String, val message: String ) {
  val dateTimeFormat = new SimpleDateFormat("dd/MM/yyyy:HH:mm:ss")
  val dateTime: Date = dateTimeFormat.parse(messageDateTime)
}

class DynamicHandler( override val name: String, override val configuration: Configuration ) extends ContextHandler {
  val dataPath: String = getHandlerParam( "JSONData" ) match { case Some ( value ) => value; case None => println("[ERROR]: Couldn't load" + "data.json" ); System.exit(1); "" }

  override def initialize(): Unit = { println("[INFO]: Dynamic handler initialized")}
  override def shutdown(): Unit = { println("[INFO]: Dynamic handler is shutting down")}

  override def handleRequest(request: Request): Option[Response] = {
    request.getRequestMethod match {
      case RequestMethod.Invalid => super.errorResponse(405)
      case RequestMethod.GET =>  handleGET( request )
    }
  }

  def handleGET ( request: Request ): Option[Response] =
    request.getRequestedURL match {
      case Some( url )
        if url == "/" =>
        sendIndexHtml
      case Some( url )
        if url == "/index.js" =>
        sendIndexJS
      case Some( url )
        if url == "/data" =>
        parseData
      case Some ( url )
        if url == "/clear" =>
        clearData()
        Option.apply(new Response(200, "text/plain", "Message was delivered".getBytes() ))
      case Some( url )
        if url.startsWith("/username=") =>
        saveNewInput( getUsername(request.getRequestedURL.get), getMessage(request.getRequestedURL.get), new Date())
        Option.apply(new Response(200, "text/plain", "Message was delivered".getBytes() ))
      case _ =>
        super.errorResponse(404 )
    }

  def saveNewInput ( username: String, message: String, date: Date ): Unit = {
    val dateTimeFormat = new SimpleDateFormat("dd/MM/yyyy:HH:mm:ss")
    val content: ArrayBuffer[Data] = readJSON(dataPath)
    println("username: " + username + ", message: " + message )
    content.addOne( new Data( username, dateTimeFormat.format(new Date), message) )
    writeJSON( dataPath, content )
  }

  def clearData(): Unit = {
    val printWriter = new PrintWriter( new File( dataPath ))
    printWriter.println()
    printWriter.close()
  }
  def getUsername ( url: String ): String = {
    if( url.split("/message=")(0).split("/username=").length == 2 ) url.split("/message=")(0).split("/username=")(1).replace("%20", " ")
    else ""
  }
  def getMessage ( url: String ): String = {
    if( url.split("/message=").length > 1 ) url.split("/message=")(1).replace("%20", " ")
    else ""
  }
  def sendIndexJS: Option[Response] = {
    val defaultFile: String = getHandlerParam( "javaScriptFile" ) match { case Some ( value ) => value; case None => println("[ERROR]: Couldn't load" + "index.js" ); System.exit(1); "" }
    val text = Source.fromFile(defaultFile).mkString
    Option.apply(new Response(200, "application/javascript", text.getBytes() ))
  }

  def sendIndexHtml: Option[Response] = {
    val defaultFile: String = getHandlerParam( "defaultFile" ) match { case Some ( value ) => value; case None => println("[ERROR]: Couldn't load" + "index.html" ); System.exit(1); "" }
    val text = Source.fromFile(defaultFile).mkString
    Option.apply(new Response(200, "text/html", text.getBytes() ))
  }

  def parseData: Option[Response] = {
    val text = Source.fromFile(dataPath).mkString
    Option.apply(new Response(200, "application/json", text.getBytes() ))
  }

  def readJSON ( path: String ): ArrayBuffer[Data] = {
    val tokens = Tokenizer.toTokens(  Source.fromFile(dataPath).mkString.replace("\r\n", "\n" ) )

    @scala.annotation.tailrec
    def parseData(input: List[Token], output: ArrayBuffer[Data] ): ArrayBuffer[Data] =
      input match {
        case OpenBrace () :: NewLine () ::
          Quoted ( keyword1 ) :: Column () :: Quoted ( value1 ) :: Comma () :: NewLine () ::
          Quoted ( keyword2 ) :: Column () :: Quoted ( value2 ) :: Comma () :: NewLine () ::
          Quoted ( keyword3 ) :: Column () :: Quoted ( value3 ) :: NewLine () ::
          CloseBrace () :: NewLine() :: tail
          if keyword1.toLowerCase == "username" && keyword2.toLowerCase == "date" && keyword3.toLowerCase == "message" =>
          output.addOne( new Data( value1, value2, value3 ) )
          parseData( tail, output )
  ;      case OpenBrace () :: NewLine () ::
          Quoted ( keyword1 ) :: Column () :: Quoted ( value1 ) :: Comma () :: NewLine () ::
          Quoted ( keyword2 ) :: Column () :: Quoted ( value2 ) :: Comma () :: NewLine () ::
          Quoted ( keyword3 ) :: Column () :: Quoted ( value3 ) :: NewLine () ::
          Stuff ( value ) :: NewLine() :: tail
          if keyword1.toLowerCase == "username" && keyword2.toLowerCase == "date" && keyword3.toLowerCase == "message" && value == "}," =>
          output.addOne( new Data( value1, value2, value3 ) )
          parseData( tail, output )
        case NewLine () :: tail =>
          parseData( tail, output )
        case Comma () :: tail =>
          parseData( tail, output )
        case OpenSquareBrace () :: tail =>
          parseData( tail, output )
        case CloseSquareBrace () :: tail =>
          parseData( tail, output )
        case Nil => output
      }
    parseData( tokens, new ArrayBuffer[Data]() )
  }

  def writeJSON ( path: String, content: ArrayBuffer[Data] ): Unit = {
    def quoted ( str: String ): String = "\"" + str + "\""
    def writeTriple ( username: String, date: String, message: String ): String =
      "\t" + "{" + "\n" +
        "\t" + "\t" + quoted("username") + ": " + quoted( username ) + ",\n" +
        "\t" + "\t" + quoted( "date")    + ": " + quoted( date)      + ",\n" +
        "\t" + "\t" + quoted("message")  + ": " + quoted( message )  + "\n"  +
        "\t" + "}"
    @scala.annotation.tailrec
    def writeAll(contentJSON: List[Data], writer: PrintWriter ): Unit =
      contentJSON match {
        case head::tail =>
          val extraChar = if( tail.isEmpty ) "" else ","
          writer.println(writeTriple( head.name, head.messageDateTime, head.message ) + extraChar + "\n" )
          writeAll( tail, writer )
        case Nil =>
      }
    val printWriter = new PrintWriter(new File( path ))
    printWriter.println("[")
    writeAll( content.toList, printWriter )
    printWriter.println("]")
    printWriter.close()
  }
}