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
                Token(LeftParen, "(", None, 3),
                Token(LeftParen, "(", None, 3),
                Token(RightParen, ")", None, 3),
                Token(RightParen, ")", None, 3),
                Token(LeftBrace, "{", None, 3),
                Token(RightBrace, "}", None, 3),
                Token(Bang, "!", None, 4),
                Token(Star, "*", None, 4),
                Token(Plus, "+", None, 4),
                Token(Minus, "-", None, 4),
                Token(Slash, "/", None, 4),
                Token(Equal, "=", None, 4),
                Token(Less, "<", None, 4),
                Token(Greater, ">", None, 4),
                Token(LessEqual, "<=", None, 4),
                Token(EqualEqual, "==", None, 4),
                Token(Number, "123.45", Some(123.45), 5),
                Token(Var, "var", None, 6),
                Token(Identifier, "a_b", None, 6),
                Token(Equal, "=", None, 6),
                Token(Number, "1", Some(1), 6),
                Token(EOF, "", None, 7)
            )
        )
    }
