package lox

import org.slf4j.LoggerFactory
import scala.util.{Failure, Success}

private val logger = LoggerFactory.getLogger(this.getClass)

def report(line: Int, where: String, message: String) =
    logger.error(s"[line $line] Error: $where: $message")

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

    private def run(input: String) =
        val scanner = Scanner(input)
        val tokens = scanner.scanTokens()

        Parser(tokens).parse() match
            case Success(expressions) =>
                // astPrinter.toString(expressions)

                for expr <- expressions do
                    val value = interpreter.evaluate(expr)
                    println(stringify(value))

            case Failure(exception) =>
                logger.error("Parsing failed: " + exception.getMessage)

@main def run(args: String*) =
    if args.length > 1 then println("Usage: lox [script]")
    else if args.length == 1 then
        val fileName = args(0)
        Lox().runFile(fileName)
    else Lox().runPrompt()
