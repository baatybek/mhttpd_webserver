import java.io.{BufferedReader, DataOutputStream, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket}
import java.util.concurrent.Executors

import NetworkService.Request

object Server {

  def getContextHandler ( configuration: Configuration, path: String ): Option[ContextHandler] = {
    val handlerName = configuration.getContextHandler(path)
    handlerName match {
      case None => None
      case Some(name) =>  Option.apply( configuration.getHandlerFactory( name )() )
    }
  }

  def serve(clientSocket: Socket, configuration: Configuration ): Unit = {
    val request: Request = new Request( clientSocket )
    val handlerOption: Option[ContextHandler] = getContextHandler( configuration, request.getRequestedURL.get )
    val handler = handlerOption.get
    handler.initialize()
    val response =  handler.handleRequest(request)
    if( response.isDefined ) {
      response.get.sendHeaders( new PrintWriter( clientSocket.getOutputStream ))
      response.get.sendContent( new DataOutputStream( clientSocket.getOutputStream ))
    }
    handler.shutdown()
  }

  def runForever( configuration: Configuration): Unit = {
    val port = configuration.getServerParam("Port").get.toInt
    val threadPoolSize = configuration.getServerParam( "ThreadPool" ).get.toInt
    val threadPool = Executors.newFixedThreadPool( threadPoolSize )
    val serverSocket: ServerSocket = new ServerSocket( port )
    while (true) {
      val clientSocket = serverSocket.accept()
      val thread = new Thread {
        serve( clientSocket, configuration)
        clientSocket.close()
      }
      threadPool.execute(thread)
    }
  }
}