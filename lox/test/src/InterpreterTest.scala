package lox

import scala.util.Success
import scala.util.Failure

class InterpreterTest extends munit.FunSuite:
    test("test print") {
        val source = """
        |print 1 + 2;
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        val parser = Parser(tokens)
        val interpreter = Interpreter()

        parser.parse() match
            case Failure(e) => fail(s"Unexpected parsing error: ${e.getMessage}")
            case Success(statements) =>
                interpreter.interpret(statements) match
                    case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                    case Success(_) => ()
    }
