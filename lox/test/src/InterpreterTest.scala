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

    test("scopes") {
        val source = """
        |{
        |  var a = 1;
        |  {
        |    var a = 2;
        |    print a;
        |  }
        |  print a;
        |}
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get
        val interpreter = Interpreter()

        val printer = AstPrinter().toString(ast)
        assertEquals(printer, "(block (var a 1.0) (block (var a 2.0) (print (a))) (print (a)))")

        val outputCapture = new java.io.ByteArrayOutputStream()

        Console.withOut(outputCapture) {
            interpreter.interpret(ast) match
                case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                case Success(_) => assertEquals(outputCapture.toString.trim, "2.0\n1.0")
        }
    }

    test("for loop") {
        val source = """
        |{
        |  var i = 0;
        |  for (;i < 5; i = i + 1) {
        |    print i;
        |  }
        |}
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get
        val interpreter = Interpreter()

        val outputCapture = new java.io.ByteArrayOutputStream()

        Console.withOut(outputCapture) {
            interpreter.interpret(ast) match
                case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                case Success(_) =>
                    assertEquals(outputCapture.toString.trim, "0.0\n1.0\n2.0\n3.0\n4.0")
        }
    }
