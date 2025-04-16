package lox

import scala.util.Failure

class ParserTest extends munit.FunSuite:
    test("error on expression") {
        val source = """
        |(123.45 > x
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()

        Parser(tokens)
            .parse() match
            case Failure(e) =>
                assertEquals(e.getMessage, "Expect expression.")
            case _ => fail("Expected a parsing error")
    }

    test("boolean") {
        val source = """
        |true;
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        val ast = Parser(tokens).parse().get

        assertEquals(
            ast,
            List(
                Stmt.Expression(
                    Expr.Literal(true)
                )
            )
        )
    }

    test("variable declaration") {
        val source = """
        |var x = 123.45;
        |var y = 1 + 1;
        |var z = x + 1;
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        val ast = Parser(tokens).parse().get

        assertEquals(
            ast,
            List(
                Stmt.Var(
                    Token(TokenType.Identifier, "x", null, 2),
                    Some(Expr.Literal(123.45))
                ),
                Stmt.Var(
                    Token(TokenType.Identifier, "y", null, 3),
                    Some(Expr.Binary(Expr.Literal(1), Token(TokenType.Plus, "+", null, 3), Expr.Literal(1)))
                ),
                Stmt.Var(
                    Token(TokenType.Identifier, "z", null, 4),
                    Some(
                        Expr.Binary(
                            Expr.Variable(
                                Token(TokenType.Identifier, "x", null, 4)
                            ),
                            Token(TokenType.Plus, "+", null, 4),
                            Expr.Literal(1)
                        )
                    )
                )
            )
        )
    }
