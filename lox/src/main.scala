package lox

def stringify(value: Any): String =
    value match
        case null      => "nil"
        case v: Double => v.toString
        case _         => value.toString

class Lox:
    // private val astPrinter = AstPrinter()
    private val interpreter = Interpreter()

    def runPrompt(): Unit =
        while true do
            print("> ")
            val input = scala.io.StdIn.readLine()
            if input.isEmpty then return
            run(input)

    def runFile(fileName: String): Unit =
        val source = scala.io.Source.fromFile(fileName).mkString
        run(source)
        if Lox.hadError then System.exit(65)
        if Lox.hadRuntimeError then System.exit(70)

    private def run(input: String): Unit =
        val tokens = Scanner(input).scanTokens()

        Parser(tokens)
            .parse()
            .map: expressions =>
                for expr <- expressions do
                    val value = interpreter.evaluate(expr)
                    println(stringify(value))

object Lox:
    var hadError = false
    var hadRuntimeError = false

    def error(line: Int, message: String): Unit =
        Lox.reportError(line, "", message)

    def error(token: Token, message: String): Unit =
        if token.tokenType == TokenType.EOF then Lox.reportError(token.line, " at end", message)
        else Lox.reportError(token.line, s" at '${token.lexeme}'", message)

    def error(err: RuntimeError): Unit =
        hadRuntimeError = true
        Lox.reportError(err.token.line, "", err.message)

    def reportError(line: Int, where: String, message: String): Unit =
        hadError = true
        println(s"[line $line] Error: $where: $message")

@main def run(args: String*) =
    if args.length > 1 then println("Usage: lox [script]")
    else if args.length == 1 then
        val fileName = args(0)
        Lox().runFile(fileName)
    else Lox().runPrompt()
