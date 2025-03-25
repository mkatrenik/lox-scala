package lox

class ScannerTest extends munit.FunSuite:
    test("scanner test") {
        import TokenType.*
        val source = """
        |// this is a comment
        |(( )){} // grouping stuff
        |!*+-/=<> <= == // operators
        |123.45 // number
        |var a_b = 1
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        assertEquals(
            tokens,
            List(
                Token(LeftParen, "(", null, 3),
                Token(LeftParen, "(", null, 3),
                Token(RightParen, ")", null, 3),
                Token(RightParen, ")", null, 3),
                Token(LeftBrace, "{", null, 3),
                Token(RightBrace, "}", null, 3),
                Token(Bang, "!", null, 4),
                Token(Star, "*", null, 4),
                Token(Plus, "+", null, 4),
                Token(Minus, "-", null, 4),
                Token(Slash, "/", null, 4),
                Token(Equal, "=", null, 4),
                Token(Less, "<", null, 4),
                Token(Greater, ">", null, 4),
                Token(LessEqual, "<=", null, 4),
                Token(EqualEqual, "==", null, 4),
                Token(Number, "123.45", 123.45, 5),
                Token(Var, "var", null, 6),
                Token(Identifier, "a_b", null, 6),
                Token(Equal, "=", null, 6),
                Token(Number, "1", 1, 6),
                Token(EOF, "", null, 7)
            )
        )
    }
