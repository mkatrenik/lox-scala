package lox

import scala.collection.mutable

enum TokenType:
    // Single-character tokens
    case LeftParen, RightParen, LeftBrace, RightBrace,
        Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
        // One or two character tokens
        Bang, BangEqual,
        Equal, EqualEqual,
        Less, LessEqual,
        Greater, GreaterEqual,
        // Literals
        Identifier, String, Number,
        // Keywords
        And, Class, Else, False, Fun, For, If, Nil, Or,
        Print, Return, Super, This, True, Var, While,
        // End of file
        EOF

case class Token(tokenType: TokenType, lexeme: String, literal: Any, line: Int)
object Token:
    def apply(tokenType: TokenType): Token = Token(tokenType, "", null, 0)

class Scanner(val source: String):
    val tokens = mutable.ListBuffer[Token]()
    // The current position in the source code
    private var current = 0
    // The start of the current lexeme
    private var start = 0
    // The current line number
    private var line = 1

    private def isAtEnd = current >= source.length

    def scanTokens(): List[Token] =
        while !isAtEnd do
            start = current
            scanToken()

        tokens += Token(TokenType.EOF, "", null, line)
        tokens.toList

    private def scanToken(): Unit =
        import TokenType.*
        val c = advance()
        c match
            case '(' => addToken(LeftParen)
            case ')' => addToken(RightParen)
            case '{' => addToken(LeftBrace)
            case '}' => addToken(RightBrace)
            case ',' => addToken(Comma)
            case '.' => addToken(Dot)
            case '-' => addToken(Minus)
            case '+' => addToken(Plus)
            case ';' => addToken(Semicolon)
            case '*' => addToken(Star)
            case '!' =>
                if `match`('=') then addToken(BangEqual)
                else addToken(Bang)
            case '=' =>
                if `match`('=') then addToken(EqualEqual)
                else addToken(Equal)
            case '>' =>
                if `match`('=') then addToken(GreaterEqual)
                else addToken(Greater)
            case '<' =>
                if `match`('=') then addToken(LessEqual)
                else addToken(Less)
            case '/' =>
                if `match`('/') then
                    // A comment goes until the end of the line.
                    while peek() != '\n' && !isAtEnd do advance()
                else addToken(Slash)
            case ' ' | '\r' | '\t' => ()
            case '\n'              => line += 1
            case '"'               => string()
            case _: Char =>
                if c.isDigit then number()
                else if c.isAlpha then identifier()
                else
                    Lox.error(line, s"Unexpected character: $c")
                    ()

    private def advance(): Char =
        val c = source(current)
        current += 1
        c

    private def addToken(tokenType: TokenType, literal: Any = null): Unit =
        tokens += Token(tokenType, source.substring(start, current), literal, line)

    private def `match`(expected: Char): Boolean =
        if isAtEnd then return false
        if source(current) != expected then return false
        current += 1
        true

    private def peek(skip: Int = 0): Char =
        if isAtEnd || ((current + skip) >= source.length()) then return '\u0000'
        source(current + skip)

    private def string(): Unit =
        while peek() != '"' && !isAtEnd do
            if peek() == '\n' then line += 1
            advance()

        if isAtEnd then
            Lox.error(line, s"Unterminated string")
            return
        // The closing quote.
        advance()
        // The closing quote is not included in the string literal.
        val value = source.substring(start + 1, current - 1)
        addToken(TokenType.String, value)

    private def number(): Unit =
        while peek().isDigit do advance()
        // Look for a fractional part.
        if peek() == '.' && peek(1).isDigit then
            // Consume the "."
            advance()
            while peek().isDigit do advance()
        addToken(TokenType.Number, source.substring(start, current).toDouble)

    private def identifier(): Unit =
        while peek().isAlphaNumeric do advance()
        val text = source.substring(start, current)
        val tokenType = text match
            case "and"    => TokenType.And
            case "class"  => TokenType.Class
            case "else"   => TokenType.Else
            case "false"  => TokenType.False
            case "for"    => TokenType.For
            case "fun"    => TokenType.Fun
            case "if"     => TokenType.If
            case "nil"    => TokenType.Nil
            case "or"     => TokenType.Or
            case "print"  => TokenType.Print
            case "return" => TokenType.Return
            case "super"  => TokenType.Super
            case "this"   => TokenType.This
            case "true"   => TokenType.True
            case "var"    => TokenType.Var
            case "while"  => TokenType.While
            case _        => TokenType.Identifier
        addToken(tokenType)

extension (c: Char)
    def isAlpha: Boolean = c.isLetter || c == '_'
    def isAlphaNumeric: Boolean = c.isAlpha || c.isDigit
