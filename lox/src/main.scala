package lox

import scala.util.Try

def stringify(value: Any): String =
    value match
        case null      => "nil"
        case v: Double => v.toString
        case _         => value.toString

class Lox:
    // private val astPrinter = AstPrinter()
    private val interpreter = Interpreter()

    def runPrompt(): Unit =
        val inputBuffer = new StringBuilder()
        while true do
            val prompt = if inputBuffer.isEmpty then "> " else "| "
            print(prompt)

            val line = scala.io.StdIn.readLine()

            if line eq null then // EOF (Ctrl+D)
                return
            else if line.isEmpty && !inputBuffer.isEmpty then
                // Empty line after some input: execute the buffer
                run(inputBuffer.toString)
                inputBuffer.clear()
                Lox.hadError = false
            else if line.nonEmpty || inputBuffer.nonEmpty then
                // Non-empty line, or first empty line (ignore first empty line)
                if !inputBuffer.isEmpty then inputBuffer.append("\n")
                inputBuffer.append(line)

    def runFile(fileName: String): Unit =
        val source = scala.io.Source.fromFile(fileName).mkString
        run(source)
        if Lox.hadError then System.exit(65)
        if Lox.hadRuntimeError then System.exit(70)

    private def run(input: String): Unit =
        val tokens = Scanner(input).scanTokens()

        val ast: Try[List[Stmt]] = Parser(tokens).parse()

        if Lox.hadError then return

        Resolver(interpreter).resolve(ast.get)

        if Lox.hadError then return

        ast.map: statements =>
            interpreter
                .interpret(statements)
                .recover:
                    case err: RuntimeError =>
                        Lox.error(err)

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
    // Lox().runFile("test.lox")
    // throw new RuntimeException("Test failed")
    if args.length > 1 then println("Usage: lox [script]")
    else if args.length == 1 then
        val fileName = args(0)
        Lox().runFile(fileName)
    else Lox().runPrompt()
