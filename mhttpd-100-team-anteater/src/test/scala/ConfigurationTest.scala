import org.scalatest.FunSuite

class ConfigurationTest extends FunSuite {

  test( "Token contents are correct for reading Quoted") {
    val quoted: String = "\"Hello World!\""
    assert( Tokenizer.toTokens(quoted) == List ( Quoted("Hello World!") ) )
  }

  test( "Token contents are correct for reading port") {
    val port: String = "Port 8080"
    assert( Tokenizer.toTokens(port) == List ( Word("Port"), Stuff("8080") ) )
  }

  test( "Token contents are correct for reading Param") {
    val param: String = "Param root docs/"
    assert( Tokenizer.toTokens(param) == List ( Word("Param"), Word("root"), Stuff("docs/") ) )
  }

  test("Token contents are correct for Handler Param") {
    val handlerParams: String = "Handler staticHandler StaticHandler {\nParam Ignore static/ignore\n}"
    assert( Tokenizer.toTokens(handlerParams) ==
            List ( Word("Handler"), Word("staticHandler"), Word("StaticHandler"), OpenBrace(), NewLine(),
                   Word("Param"), Word("Ignore"), Stuff("static/ignore"), NewLine(), CloseBrace() ) )
  }

  test("Test Param Configuration" ) {
    val param: String = "Param root docs/\n"
    assert( Parser.parse( Tokenizer.toTokens(param) ) == List( Param("root", "docs/") ) )
  }

  test( "Test Context Configuration" ) {
    val context: String = "Context /static staticHandler\n"
    assert( Parser.parse( Tokenizer.toTokens(context) ) == List( Context("/static", "staticHandler" ) ) )
  }

  test( "HandlerSpec for Configuration" ) {
    val handler: String = "Handler staticHandler StaticHandler {\n}\n"
    val handlerClass = Class.forName("StaticHandler").asInstanceOf[Class[ContextHandler]]
    assert( Parser.parse( Tokenizer.toTokens(handler) ) == List( HandlerSpec("staticHandler", handlerClass, Nil) ))
  }

  test("getServerParam from Configuration" ) {
    val param: String = "Param root docs/\n"
    val config: Configuration = new OptionConfiguration(Parser.parse( Tokenizer.toTokens(param) ) )
    assert( config.getServerParam("root").contains("docs/") )
  }

  test("getHandlerParam from Configuration" ) {
    val configParams = "Handler staticHandler StaticHandler {\n" + "Param root docs/\n" + "}\n" + "Context /static staticHandler\n"
    val config: Configuration = new OptionConfiguration(Parser.parse( Tokenizer.toTokens(configParams) ))
    assert( config.getHandlerParam("staticHandler", "root").contains("docs/") )
  }

  test("getHandlerName for context from Configuration") {
    val configParams = "Handler staticHandler StaticHandler {\n" + "}\n" + "Context /static staticHandler\n"
    val configuration: Configuration = new OptionConfiguration(Parser.parse( Tokenizer.toTokens(configParams) ))
    val handlerName = configuration.getContextHandler("/static/")
    assert( handlerName.contains("staticHandler") )
  }

  test("contextHandlerFactory from Name") {
    val configParams = "Param DocumentRoot docs/\n" +
                       "Param ErrorDocument403 403\nParam ErrorDocument404 404\nParam ErrorDocument405 405\nParam ErrorDocument500 500\n" +
                       "Handler staticHandler StaticHandler {\n" + "}\n" + "Context /static staticHandler\n"
    val configuration: Configuration = new OptionConfiguration(Parser.parse( Tokenizer.toTokens(configParams) ))
    val handler = configuration.getHandlerFactory("staticHandler" )()
    assert( handler.name == "staticHandler" && handler.configuration == configuration )
  }
}
