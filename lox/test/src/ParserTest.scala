package lox

class ParserTest extends munit.FunSuite:
    test("error test") {
        val source = """
        |(123.45 > x
        """.stripMargin
        val scanner = Scanner(source)
        val tokens = scanner.scanTokens()
        print(tokens)
        tokens match
            case i: List[?] =>
                Parser(i).parse() match
                    case scala.util.Success(_) =>
                        fail("Expected a parsing error")
                    case scala.util.Failure(exception) =>
                        assertEquals(exception.getMessage, "Expect expression.")
            case _ =>
                fail("Expected a list of tokens")
    }
