package lox

import scala.util.boundary, boundary.break
import scala.util.{Failure, Try}
import scala.util.Success

final class Parser(tokens: List[Token]):

    private var current = 0

    def parse(): Try[List[Stmt]] =
        val statements = List.newBuilder[Stmt]
        Try:
            while !isAtEnd() do
                val decl = Try(declaration())
                decl match
                    case Success(d) => statements += d
                    case Failure(e: ParseError) =>
                        reportError(tokens(current).line, "Parse error", e.message)
                        synchronize()
                    case Failure(e) =>
                        throw e
            statements.result()

    def reportError(line: Int, where: String, message: String) =
        println(s"[line $line] Parsing error: $where: $message")

    private def isAtEnd(): Boolean =
        current >= tokens.length || tokens(current).tokenType == TokenType.EOF

    /** check if the next token is one of the given token types and advance the current token if it
      * is.
      */
    private def `match`(tokenTypes: TokenType*): Boolean =
        boundary:
            for tokenType <- tokenTypes do
                if check(tokenType) then
                    advance()
                    break(true)
            false

    /** check if the current token is the given token type
      */
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

    private def declaration(): Stmt =
        if `match`(TokenType.Class) then classDeclaration()
        else if `match`(TokenType.Fun) then funDeclaration(FunctionType.Function)
        else if `match`(TokenType.Var) then varDeclaration()
        else statement()

    private def classDeclaration(): Stmt =
        val name = consume(TokenType.Identifier, "Expect class name.")
        var superclass: Option[Expr.Variable] = None
        if `match`(TokenType.Less) then
            consume(TokenType.Identifier, "Expect superclass name.")
            superclass = Some(Expr.Variable(previous()))
        consume(TokenType.LeftBrace, "Expect '{' before class body.")
        val methods = scala.collection.mutable.ArrayBuffer[Stmt.Function]()
        while !check(TokenType.RightBrace) && !isAtEnd() do
            methods += funDeclaration(FunctionType.Method)
        consume(TokenType.RightBrace, "Expect '}' after class body.")
        Stmt.Class(name, superclass, methods.toList)

    private def funDeclaration(
        kind: FunctionType.Function.type | FunctionType.Method.type
    ): Stmt.Function =
        val name = consume(TokenType.Identifier, s"Expect $kind name.")
        consume(TokenType.LeftParen, s"Expect '(' after name.")
        val arguments = scala.collection.mutable.ArrayBuffer[Token]()
        if !check(TokenType.RightParen) then
            boundary:
                while arguments.length < 255 && !`match`(TokenType.RightParen) do
                    arguments += consume(TokenType.Identifier, "Expect parameter name.")
                    if `match`(TokenType.Comma) then ()
                    else break()

        consume(TokenType.RightParen, "Expect ')' after parameters.")
        consume(TokenType.LeftBrace, s"Expect '{' before $kind body.")
        val body = block()
        Stmt.Function(name, arguments.toList, body)

    private def varDeclaration(): Stmt =
        val name = consume(TokenType.Identifier, "Expect variable name.")
        var initializer: Option[Expr] = None
        if `match`(TokenType.Equal) then initializer = Some(expression())
        consume(TokenType.Semicolon, "Expect ';' after variable declaration.")
        Stmt.Var(name, initializer)

    private def statement(): Stmt =
        if `match`(TokenType.LeftBrace) then Stmt.Block(block())
        else if `match`(TokenType.Print) then printStatement()
        else if `match`(TokenType.If) then ifStatement()
        else if `match`(TokenType.While) then whileStatement()
        else if `match`(TokenType.For) then forStatement()
        else if `match`(TokenType.Return) then returnStatement()
        else expressionStatement()

    private def returnStatement(): Stmt =
        val keyword = previous()
        var value: Option[Expr] = None
        if !check(TokenType.Semicolon) then value = Some(expression())
        consume(TokenType.Semicolon, "Expect ';' after return value.")
        Stmt.Return(keyword, value)

    private def block(): List[Stmt] =
        val statements = List.newBuilder[Stmt]
        while !isAtEnd() && !check(TokenType.RightBrace) do statements += declaration()
        consume(TokenType.RightBrace, "Expect '}' after block.")
        statements.result()

    private def ifStatement(): Stmt =
        consume(TokenType.LeftParen, "Expect '(' after 'if'.")
        val condition = expression()
        consume(TokenType.RightParen, "Expect ')' after if condition.")
        val thenBranch = statement()
        val elseBranch =
            if `match`(TokenType.Else) then Some(statement())
            else None
        Stmt.If(condition, thenBranch, elseBranch)

    private def whileStatement(): Stmt =
        consume(TokenType.LeftParen, "Expect '(' after 'while'.")
        val condition = expression()
        consume(TokenType.RightParen, "Expect ')' after while condition.")
        val body = statement()
        Stmt.While(condition, body)

    private def forStatement(): Stmt =
        consume(TokenType.LeftParen, "Expect '(' after 'for'.")
        val initializer =
            if `match`(TokenType.Semicolon) then None
            else if `match`(TokenType.Var) then Some(varDeclaration())
            else Some(expressionStatement())

        var condition =
            if check(TokenType.Semicolon) then None
            else Some(expression())
        consume(TokenType.Semicolon, "Expect ';' after loop condition.")

        val increment =
            if check(TokenType.RightParen) then None
            else Some(expression())
        consume(TokenType.RightParen, "Expect ')' after for clauses.")

        var body = statement()

        body = increment match
            case Some(increment) =>
                Stmt.Block(List(body, Stmt.Expression(increment)))
            case None => body

        condition = condition.orElse(Some(Expr.Literal(true)))

        body = Stmt.While(condition.get, body)

        initializer match
            case Some(initializer) =>
                body = Stmt.Block(List(initializer, body))
            case None => ()

        body

    private def printStatement(): Stmt =
        val value = expression()
        consume(TokenType.Semicolon, "Expect ';' after value.")
        Stmt.Print(value)

    private def expressionStatement(): Stmt =
        val expr = expression()
        consume(TokenType.Semicolon, "Expect ';' after expression.")
        Stmt.Expression(expr)

    private def expression(): Expr =
        assignment()

    private def logicalOr(): Expr =
        var expr = logicalAnd()
        while `match`(TokenType.Or) do
            val operator = previous()
            val right = logicalAnd()
            expr = Expr.Logical(expr, operator, right)
        expr

    private def logicalAnd(): Expr =
        var expr = equality()
        while `match`(TokenType.And) do
            val operator = previous()
            val right = equality()
            expr = Expr.Logical(expr, operator, right)
        expr

    private def assignment(): Expr =
        val expr = logicalOr()
        if `match`(TokenType.Equal) then
            val equals = previous()
            val value = assignment()
            expr match
                case Expr.Variable(name) =>
                    return Expr.Assign(name, value)
                case Expr.Get(objectExpr, name) =>
                    return Expr.Set(objectExpr, name, value)
                case _ =>
                    throw error(equals, "Invalid assignment target.")
        expr

    private def equality(): Expr =
        var expr = comparison()
        while `match`(TokenType.BangEqual, TokenType.EqualEqual) do
            val operator = previous()
            val right = comparison()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def comparison(): Expr =
        var expr = term()
        while `match`(
                TokenType.Greater,
                TokenType.GreaterEqual,
                TokenType.Less,
                TokenType.LessEqual
            )
        do
            val operator = previous()
            val right = term()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def term(): Expr =
        var expr = factor()
        while `match`(TokenType.Minus, TokenType.Plus) do
            val operator = previous()
            val right = factor()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def factor(): Expr =
        var expr = unary()
        while `match`(TokenType.Slash, TokenType.Star) do
            val operator = previous()
            val right = unary()
            expr = Expr.Binary(expr, operator, right)
        expr

    private def unary(): Expr =
        if `match`(TokenType.Bang, TokenType.Minus) then
            val operator = previous()
            val right = unary()
            Expr.Unary(operator, right)
        else call()

    private def call(): Expr =
        var expr = primary()
        boundary:
            while true do
                if `match`(TokenType.LeftParen) then expr = finishCall(expr)
                else if `match`(TokenType.Dot) then
                    val name = consume(TokenType.Identifier, "Expect property name after '.'.")
                    expr = Expr.Get(expr, name)
                else break()
        expr

    private def finishCall(callee: Expr): Expr =
        val arguments = scala.collection.mutable.ArrayBuffer[Expr]()
        if !check(TokenType.RightParen) then
            arguments += expression()
            while `match`(TokenType.Comma) do
                if arguments.length >= 255 then
                    error(peek(), "Cannot have more than 255 arguments.")
                arguments += expression()
        val paren = consume(TokenType.RightParen, "Expect ')' after arguments.")
        Expr.Call(callee, paren, arguments.toList)

    private def primary(): Expr =
        if `match`(TokenType.False) then Expr.Literal(false)
        else if `match`(TokenType.True) then Expr.Literal(true)
        else if `match`(TokenType.Nil) then Expr.Literal(Nil)
        else if `match`(TokenType.Number, TokenType.String) then
            val value = previous().literal
            value
                .map(Expr.Literal(_))
                .getOrElse(
                    throw error(previous(), "Expect literal value.")
                )
        else if `match`(TokenType.LeftParen) then
            val expr = expression()
            consume(TokenType.RightParen, "Expect ')' after expression.")
            Expr.Grouping(expr)
        else if `match`(TokenType.Identifier) then Expr.Variable(previous())
        else if `match`(TokenType.Super) then
            val keyword = previous()
            consume(TokenType.Dot, "Expect '.' after 'super'.")
            val method = consume(TokenType.Identifier, "Expect superclass method name.")
            Expr.Super(keyword, method)
        else if `match`(TokenType.This) then Expr.This(previous())
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
            if `match`(
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
