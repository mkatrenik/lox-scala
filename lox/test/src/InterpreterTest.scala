package lox

import scala.util.Success
import scala.util.Failure

class InterpreterTest extends munit.FunSuite:
    test("test print") {
        val source = """
        |print 1 + 2;
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get
        val interpreter = Interpreter()

        val printer = AstPrinter().toString(ast)
        assertEquals(printer, "(print (+ 1.0 2.0))")

        interpreter.interpret(ast) match
            case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
            case Success(_) => ()
    }
