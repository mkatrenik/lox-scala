package lox

import scala.util.Success
import scala.util.Failure
import pprint.pprintln

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
        Resolver(interpreter).resolve(ast)

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
        |  for (var i = 0;i < 5; i = i + 1) {
        |    print i;
        |  }
        |}
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get

        val interpreter = Interpreter()
        val resolver = Resolver(interpreter)
        resolver.resolve(ast)

        val outputCapture = new java.io.ByteArrayOutputStream()

        Console.withOut(outputCapture) {
            interpreter.interpret(ast) match
                case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                case Success(_) =>
                    assertEquals(outputCapture.toString.trim, "0.0\n1.0\n2.0\n3.0\n4.0")
        }
    }

    test("function declaration with closure") {
        val source = """
        |fun makeCounter() {
        |  var i = 0;
        |  fun count() {
        |      i = i + 1;
        |      print i;
        |  }
        |  return count;
        |}
        |var counter = makeCounter();
        |counter();
        |counter();
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get
        // pprintln(ast)
        val interpreter = Interpreter()
        val resolver = Resolver(interpreter)
        resolver.resolve(ast)

        val outputCapture = new java.io.ByteArrayOutputStream()

        Console.withOut(outputCapture) {
            interpreter.interpret(ast) match
                case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                case Success(_) => assertEquals(outputCapture.toString.trim, "1.0\n2.0")
        }
    }

    test("class declaration") {
        val source = """
        |class Foo {
        |  bar() {
        |    print "Hello from Foo!";
        |  }
        |}
        |var foo = Foo();
        |print foo;
        """.stripMargin

        val tokens = Scanner(source).scanTokens()
        val ast = Parser(tokens).parse().get
        // pprintln(ast)
        val interpreter = Interpreter()
        val resolver = Resolver(interpreter)
        resolver.resolve(ast)

        val outputCapture = new java.io.ByteArrayOutputStream()

        Console.withOut(outputCapture) {
            interpreter.interpret(ast) match
                case Failure(e) => fail(s"Unexpected interpretation error: ${e.getMessage}")
                case Success(_) => assertEquals(outputCapture.toString.trim, "<instance Foo>")
        }
    }
