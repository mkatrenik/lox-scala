package lox

import scala.util.boundary, boundary.break
import scala.util.Try

class Parser(tokens: List[Token]):

    private var current = 0

    def parse(): Try[List[Expr]] =
        val expressions = List.newBuilder[Expr]
        Try:
            while !isAtEnd() do expressions += expression()
            expressions.result()

    def reportError(line: Int, where: String, message: String) =
        println(s"[line $line] Parsing error: $where: $message")

    private def isAtEnd(): Boolean =
        current >= tokens.length || tokens(current).tokenType == TokenType.EOF

    private def matchToken(tokenTypes: TokenType*): Boolean =
        boundary:
            for tokenType <- tokenTypes do
                if check(tokenType) then
                    advance()
                    break(true)
            false

    private def check(tokenType: TokenType): Boolean =
        if isAtEnd() then false
        else tokens(current).tokenType == tokenType

    private def advance(): Token =
        if !isAtEnd() then current += 1
        tokens(current - 1)

    private def previous(): Token =
        tokens(current - 1)

    private def peek(): Token =
        if isAtEnd() then tokens(tokens.length - 1)
        else tokens(current)

    private def expression(): Expr =
        equality()

    private def equality(): Expr =
        var expr = comparison()
        while matchToken(TokenType.BangEqual, TokenType.EqualEqual) do
            val operator = previous()
            val right = comparison()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def comparison(): Expr =
        var expr = term()
        while matchToken(TokenType.Greater, TokenType.GreaterEqual, TokenType.Less, TokenType.LessEqual) do
            val operator = previous()
            val right = term()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def term(): Expr =
        var expr = factor()
        while matchToken(TokenType.Minus, TokenType.Plus) do
            val operator = previous()
            val right = factor()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def factor(): Expr =
        var expr = unary()
        while matchToken(TokenType.Slash, TokenType.Star) do
            val operator = previous()
            val right = unary()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def unary(): Expr =
        if matchToken(TokenType.Bang, TokenType.Minus) then
            val operator = previous()
            val right = unary()
            Expr.Unary(operator, right)
        else primary()

    private def primary(): Expr =
        if matchToken(TokenType.False) then Expr.Literal(false)
        else if matchToken(TokenType.True) then Expr.Literal(true)
        else if matchToken(TokenType.Nil) then Expr.Literal(null)
        else if matchToken(TokenType.Number, TokenType.String) then
            val value = previous().literal
            Expr.Literal(value)
        else if matchToken(TokenType.LeftParen) then
            val expr = expression()
            consume(TokenType.RightParen, "Expect ')' after expression.")
            Expr.Grouping(expr)

        // else if matchToken(TokenType.Identifier) then Expr.Variable(previous())
        // else if matchToken(TokenType.Super) then
        //     val keyword = previous()
        //     consume(TokenType.Dot, "Expect '.' after 'super'.")
        //     val method = consume(TokenType.Identifier, "Expect superclass method name.")
        //     Expr.Super(keyword, method)
        // else if matchToken(TokenType.This) then Expr.This(previous())
        else throw error(peek(), "Expect expression.")

    private def consume(tokenType: TokenType, message: String): Token =
        if check(tokenType) then advance()
        else throw error(previous(), message)

    private def error(token: Token, message: String): ParseError =
        Lox.error(token, message)
        val error = ParseError(message)
        error

    private def synchronize(): Unit =
        advance()
        while !isAtEnd() do
            if previous().tokenType == TokenType.Semicolon then return
            if matchToken(
                    TokenType.Class,
                    TokenType.Fun,
                    TokenType.Var,
                    TokenType.For,
                    TokenType.If,
                    TokenType.While,
                    TokenType.Print,
                    TokenType.Return
                )
            then return
            advance()
