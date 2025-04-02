package lox

import org.slf4j.LoggerFactory
import scala.util.{Failure, Success, Try}

private val logger = LoggerFactory.getLogger(this.getClass)

def report(line: Int, where: String, message: String) =
    logger.error(s"[line $line] Error: $where: $message")

@main def run =
    val scanner = Scanner("1 + 1")
    val tokens = scanner.scanTokens()
    logger.info("\n" + tokens.mkString("\n"))

    Parser(tokens).parse() match
        case Success(expressions) =>
            logger.info("Parsed expressions: " + expressions.mkString(", "))
        case Failure(exception) =>
            logger.error("Parsing failed: " + exception.getMessage)
