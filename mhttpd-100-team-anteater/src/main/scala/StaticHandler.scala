import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Date

import NetworkService.{Request, RequestMethod, Response}

class DirectoryList ( requestedPath: String ) {
  def getDirectoryContents (): Array[Byte] = {
    ( initHtml() + createDirectoryListHTML( requestedPath ) + endHtml() ).getBytes(StandardCharsets.UTF_8)
  }
  private def initHtml(): String = {
    "<!DOCTYPE html>\n" +
      "<html>\n" +
      "<head>\n" +
      "\t<style> a { color:black; } </style>"+
      "\t<title>DIRECTORY LIST</title>\n" +
      "</head>\n" +
      "<body style=\"background-color:white\">\n" +
      "\t<h1 style=\"font-size:30px;color:black;font-family:Comic Sans MS,serif\">Directory List</h1>\n" +
      "\t<ul style =\"font-size:20px;font-family:Comic Sans MS\">\n"
  }
  private def endHtml(): String = {
    "</ul>\n</body>\n</html>"
  }
  private def createDirectoryListHTML ( path: String ): String = {
    val directories: List[File] = getListOfDirectories( path )
    val files: List[File] = getListOfFiles( path )
    def directoryContents( content: List[File], acc: String ): String =
      content match {
        case Nil => acc
        case head::tail => directoryContents( tail, acc + "\t" + "<ul><a href=\"" + head.getName + "\";style=\"color:black\">" + head.getName + "</a></ul>" )
      }
    directoryContents( directories, "" ) + directoryContents( files, "" )
  }

  private def getContentList(path: String, f: File => Boolean ): List[File] = {
    val directory: File = new File( path )
    if( directory.exists() && directory.isDirectory )
      directory.listFiles.filter( f(_) ).toList
    else
      List[File]()
  }
  private def getListOfFiles ( path: String ): List[File] = getContentList( path, x => x.isFile )
  private def getListOfDirectories(path: String):List[File] = getContentList( path, x => x.isDirectory )
}

class StaticHandler ( override val name: String, override val configuration: Configuration ) extends ContextHandler {
  val ignore: String = getHandlerParam( "Ignore") match { case Some(param) => param; case None => null }
  val defaultFile: String = "index.html"

  override def initialize(): Unit = {
    println("[INFO]: Static handler initialized")
  }
  override def shutdown(): Unit = {
    println("[INFO]: Static handler is shutting down")
  }
  def getContentType ( filePath: String ): String =
    if( filePath.endsWith(".html") ) "text/html"
    else if( filePath.endsWith(".gif") ) "image/gif"
    else if( filePath.endsWith(".jpeg") ) "image/jpeg"
    else if( filePath.endsWith(".jpg") ) "image/jpg"
    else if( filePath.endsWith(".png") ) "image/png"
    else if( filePath.endsWith(".svg") ) "image/svg"
    else if( filePath.endsWith(".ico") ) "image/x-icon"
    else "text/plain"

  override def handleRequest(request: Request): Option[Response] = {

    val requestMethod: RequestMethod.Value = request.getRequestMethod
    val requestedUrl: String = request.getRequestedURL match { case Some ( url ) => url; case None => return None }

    if( requestMethod == RequestMethod.Invalid ) return super.errorResponse( 405 )

    if( ignore != null && requestedUrl.contains( ignore ) || ! new File( docRoot + requestedUrl ).exists() ) return super.errorResponse( 404 )

    if( requestedUrl.endsWith("/") ) {
      val requestedFile: File = new File( docRoot + requestedUrl + defaultFile )
      if( requestedFile.exists() ) {
        Option.apply( new Response( 200, getContentType( defaultFile ), Files.readAllBytes( Paths.get( docRoot + requestedUrl + defaultFile ) )  ) )
      } else {
        Option.apply( new Response( 200, getContentType(".html" ), new DirectoryList( docRoot + requestedUrl ).getDirectoryContents() ) )
      }
    } else {
      val requestedFile: File = new File( docRoot + requestedUrl )
      if( ! requestedFile.exists() || requestedFile.isDirectory ) return super.errorResponse( 404 )
      Option.apply(  new Response( 200, getContentType( requestedUrl ), Files.readAllBytes( Paths.get( docRoot + requestedUrl ) )  ) )
    }
  }
}