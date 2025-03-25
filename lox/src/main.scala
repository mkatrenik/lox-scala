package lox

import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger(this.getClass)

def report(line: Int, where: String, message: String) =
    logger.error(s"[line $line] Error: $where: $message")

@main def run =
    val scanner = Scanner("print \"Hello, world!\"")
    val tokens = scanner.scanTokens()
    tokens match
        case e: SyntaxError =>
            report(e.line, e.where, e.message)
            System.exit(64)
        case tokens: List[Token] => logger.info("\n" + tokens.mkString("\n"))
