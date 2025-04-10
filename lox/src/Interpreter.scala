package lox

import lox.Expr.*
import scala.util.Try
import lox.Stmt.Print
import lox.Stmt.Expression

class Interpreter extends ExprVisitor[Any], StmtVisitor[Unit]:

    def interpret(statements: List[Stmt]): Try[Unit] =
        Try:
            statements.foreach(execute(_))

    def evaluate(expr: Expr): Any = expr.accept(this)
    def execute(stmt: Stmt): Unit = stmt.accept(this)

    def runtimeError(token: Token, message: String): Nothing =
        throw RuntimeError(token, message)

    def errorNumbersOrStrings(token: Token) = runtimeError(token, "Operands must be two numbers or two strings.")
    def errorNumbers(token: Token) = runtimeError(token, "Operands must be two numbers.")

    def visitExpressionStmt(stmt: Expression): Unit =
        evaluate(stmt.expression)
        ()

    def visitPrintStmt(stmt: Print): Unit =
        val value = evaluate(stmt.expression)
        println(stringify(value))
        ()

    def visitLiteralExpr(expr: Literal): Any = expr.value.asInstanceOf[Any]
    def visitGroupingExpr(expr: Grouping): Any = evaluate(expr.expression)
    def visitUnaryExpr(expr: Unary): Any =
        val right = evaluate(expr.right)
        (expr.operator.tokenType, right) match
            case (TokenType.Minus, right: Double) => -right
            case (TokenType.Bang, right)          => !isTruthy(right)
            case _ =>
                runtimeError(
                    expr.operator,
                    s"Invalid unary operator: ${expr.operator.lexeme} for ${right.getClass.getSimpleName}"
                )

    def visitBinaryExpr(expr: Binary): Any =
        val right = evaluate(expr.right)
        val left = evaluate(expr.left)
        expr.operator.tokenType match
            case TokenType.Plus =>
                (left, right) match
                    case (l: Double, r: Double) => l + r
                    case (l: String, r: String) => l + r
                    case _                      => errorNumbersOrStrings
            case TokenType.Minus =>
                (left, right) match
                    case (l: Double, r: Double) => l - r
                    case _                      => errorNumbers
            case TokenType.Star =>
                (left, right) match
                    case (l: Double, r: Double) => l * r
                    case _                      => errorNumbers
            case TokenType.Slash =>
                (left, right) match
                    case (l: Double, r: Double) =>
                        if r == 0 then runtimeError(expr.operator, "Division by zero.")
                        l / r
                    case _ => errorNumbers
            case TokenType.Greater =>
                (left, right) match
                    case (l: Double, r: Double) => l > r
                    case _                      => errorNumbers
            case TokenType.GreaterEqual =>
                (left, right) match
                    case (l: Double, r: Double) => l >= r
                    case _                      => errorNumbers
            case TokenType.Less =>
                (left, right) match
                    case (l: Double, r: Double) => l < r
                    case _                      => errorNumbers
            case TokenType.LessEqual =>
                (left, right) match
                    case (l: Double, r: Double) => l <= r
                    case _                      => errorNumbers
            case TokenType.EqualEqual =>
                (left.nullToOption, right.nullToOption) match
                    case (None, None)                      => true
                    case (None, Some(_)) | (Some(_), None) => false
                    case (Some(l), Some(r))                => l == r
                    case _                                 => errorNumbersOrStrings
            case TokenType.BangEqual =>
                (left.nullToOption, right.nullToOption) match
                    case (None, None)                      => false
                    case (None, Some(_)) | (Some(_), None) => true
                    case (Some(l), Some(r))                => l != r
                    case _                                 => errorNumbersOrStrings
            case _ => runtimeError(expr.operator, "Invalid binary operator: " + expr.operator.lexeme)

    def visitAssignExpr(expr: Assign): Any = ???
    def visitVariableExpr(expr: Variable): Any = ???
    def visitCallExpr(expr: Call): Any = ???
    def visitLogicalExpr(expr: Logical): Any = ???
    def visitSetExpr(expr: Set): Any = ???
    def visitGetExpr(expr: Get): Any = ???
    def visitThisExpr(expr: This): Any = ???
    def visitSuperExpr(expr: Super): Any = ???
    // def visitForExpr(expr: For): Any = ???
    // def visitIfExpr(expr: If): Any = ???
    // def visitWhileExpr(expr: While): Any = ???
    // def visitBlockExpr(expr: Block): Any = ???
    // def visitPrintExpr(expr: Print): Any = ???
    // def visitReturnExpr(expr: Return): Any = ???
    // def visitBreakExpr(expr: Break): Any = ???
    // def visitContinueExpr(expr: Continue): Any = ???
    // def visitFunctionExpr(expr: Function): Any = ???
    // def visitClassExpr(expr: Class): Any = ???
    // def visitVarExpr(expr: Var): Any = ???
    // def visitImportExpr(expr: Import): Any = ???
    // def visitExpressionStmt(expr: Expression): Any = ???
    // def visitPrintStmt(expr: Print): Any = ???
    // def visitBlockStmt(expr: Block): Any = ???
    // def visitIfStmt(expr: If): Any = ???
    // def visitWhileStmt(expr: While): Any = ???
    // def visitForStmt(expr: For): Any = ???
    // def visitFunctionStmt(expr: Function): Any = ???
    // def visitClassStmt(expr: Class): Any = ???

def isTruthy(value: Any): Boolean =
    value match
        case null       => false
        case b: Boolean => b
        case _          => true
