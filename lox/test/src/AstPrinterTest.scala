package lox

class AstPrinterTest extends munit.FunSuite:
    test("scanner test") {
        val result = Expr.Binary(
            Expr.Unary(Token(TokenType.Minus, "-", null, 1), Expr.Literal(123)),
            Token(TokenType.Star, "*", null, 1),
            Expr.Grouping(Expr.Literal(45.67))
        )

        val output = AstPrinter.print(result)
        assertEquals(output, "(* (- 123) (45.67))")
    }
