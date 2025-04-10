package lox

class ParserTest extends munit.FunSuite:
    test("error test") {
        val source = """
        |(123.45 > x
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()

        Parser(tokens)
            .parse()
            .fold(
                e => assertEquals(e.getMessage, "Expect expression."),
                _ => fail("Expected a parsing error")
            )

    }

    test("test boolean") {
        val source = """
        |true;
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        Parser(tokens)
            .parse()
            .fold(
                _ => fail("Unexpected parsing error"),
                exps =>
                    assertEquals(
                        exps,
                        List(
                            Stmt.Expression(
                                Expr.Literal(true)
                            )
                        )
                    )
            )
    }
