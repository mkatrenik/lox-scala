package lox

import scala.util.Try
import java.util.UUID

final class Interpreter extends ExprVisitor[Any], StmtVisitor[Unit]:
    val globals = Environment()
    private var environment = globals
    val locals = collection.mutable.Map[UUID, Int]()

    globals.define(
        "clock",
        new LoxCallable:
            def call(interpreter: Interpreter, arguments: List[Any]): Any =
                System.currentTimeMillis() / 1000.0
            def arity: Int = 0
            override def toString: String = "<native fn>"
    )

    def interpret(statements: List[Stmt]): Try[Unit] =
        Try:
            statements.foreach(execute(_))

    def evaluate(expr: Expr): Any = expr.accept(this)
    def execute(stmt: Stmt): Unit = stmt.accept(this)

    def resolve(expr: Expr, depth: Int): Unit =
        locals.update(expr.uniqueId, depth)

    def runtimeError(token: Token, message: String): Nothing =
        throw RuntimeError(token, message)

    def errorNumbersOrStrings(token: Token) =
        runtimeError(token, "Operands must be two numbers or two strings.")
    def errorNumbers(token: Token) = runtimeError(token, "Operands must be two numbers.")

    def visitExpressionStmt(stmt: Stmt.Expression): Unit =
        evaluate(stmt.expression)
        ()

    def visitPrintStmt(stmt: Stmt.Print): Unit =
        val value = evaluate(stmt.expression)
        println(stringify(value))
        ()

    def visitLiteralExpr(expr: Expr.Literal): Any = expr.value.asInstanceOf[Any]
    def visitGroupingExpr(expr: Expr.Grouping): Any = evaluate(expr.expression)
    def visitUnaryExpr(expr: Expr.Unary): Any =
        val right = evaluate(expr.right)
        (expr.operator.tokenType, right) match
            case (TokenType.Minus, right: Double) => -right
            case (TokenType.Bang, right)          => !isTruthy(right)
            case _ =>
                runtimeError(
                    expr.operator,
                    s"Invalid unary operator: ${expr.operator.lexeme} for ${right.getClass.getSimpleName}"
                )

    def visitBinaryExpr(expr: Expr.Binary): Any =
        val right = evaluate(expr.right)
        val left = evaluate(expr.left)

        expr.operator.tokenType match
            case TokenType.Plus =>
                (left, right) match
                    case (l: Double, r: Double) => l + r
                    case (l: String, r: String) => l + r
                    case _                      => errorNumbersOrStrings(expr.operator)
            case TokenType.Minus =>
                (left, right) match
                    case (l: Double, r: Double) => l - r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.Star =>
                (left, right) match
                    case (l: Double, r: Double) => l * r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.Slash =>
                (left, right) match
                    case (l: Double, r: Double) =>
                        if r == 0 then runtimeError(expr.operator, "Division by zero.")
                        l / r
                    case _ => errorNumbers(expr.operator)
            case TokenType.Greater =>
                (left, right) match
                    case (l: Double, r: Double) => l > r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.GreaterEqual =>
                (left, right) match
                    case (l: Double, r: Double) => l >= r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.Less =>
                (left, right) match
                    case (l: Double, r: Double) => l < r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.LessEqual =>
                (left, right) match
                    case (l: Double, r: Double) => l <= r
                    case _                      => errorNumbers(expr.operator)
            case TokenType.EqualEqual =>
                (left.nullToOption, right.nullToOption) match
                    case (None, None)                      => true
                    case (None, Some(_)) | (Some(_), None) => false
                    case (Some(l), Some(r))                => l == r
                    case _                                 => errorNumbersOrStrings(expr.operator)
            case TokenType.BangEqual =>
                (left.nullToOption, right.nullToOption) match
                    case (None, None)                      => false
                    case (None, Some(_)) | (Some(_), None) => true
                    case (Some(l), Some(r))                => l != r
                    case _                                 => errorNumbersOrStrings(expr.operator)
            case _ =>
                runtimeError(expr.operator, "Invalid binary operator: " + expr.operator.lexeme)

    def visitAssignExpr(expr: Expr.Assign): Any =
        val value = evaluate(expr.value)
        locals.get(expr.uniqueId) match
            case Some(distance) =>
                environment.assignAt(distance, expr.name, value)
            case None =>
                globals.assign(expr.name, value)
        value

    def visitVariableExpr(expr: Expr.Variable): Any =
        lookupVariable(expr.name, expr)

    def lookupVariable(name: Token, expr: Expr.Variable): Any =
        locals.get(expr.uniqueId) match
            case Some(distance) =>
                environment.getAt(distance, name.lexeme)
            case None => globals.get(name)

    def visitCallExpr(expr: Expr.Call): Any =
        val callee = evaluate(expr.callee)
        val arguments = expr.arguments.map(evaluate)
        callee match
            case callable: LoxCallable =>
                if arguments.size != callable.arity then
                    runtimeError(
                        expr.paren,
                        s"Expected ${callable.arity} arguments but got ${arguments.size}."
                    )
                callable.call(this, arguments)
            case _ =>
                runtimeError(expr.paren, "Can only call functions and classes.")

    def visitLogicalExpr(expr: Expr.Logical): Any =
        val left = evaluate(expr.left)
        expr.operator.tokenType match
            case TokenType.Or =>
                if isTruthy(left) then left
                else evaluate(expr.right)
            case TokenType.And =>
                if isTruthy(left) then evaluate(expr.right)
                else left
            case _ => throw new RuntimeError(expr.operator, "Invalid logical operator.")

    def visitSetExpr(expr: Expr.Set): Any = ???
    def visitGetExpr(expr: Expr.Get): Any = ???
    def visitThisExpr(expr: Expr.This): Any = ???
    def visitSuperExpr(expr: Expr.Super): Any = ???

    def visitWhileStmt(stmt: Stmt.While): Unit =
        while isTruthy(evaluate(stmt.condition)) do execute(stmt.body)

    def visitVarStmt(stmt: Stmt.Var): Unit =
        val evaluatedValue = stmt.initializer match
            case None        => null
            case Some(value) => evaluate(value)
        environment.define(stmt.name.lexeme, evaluatedValue)

    def visitBlockStmt(expr: Stmt.Block): Unit =
        executeBlock(expr.statements, Environment(Some(environment)))

    def executeBlock(statements: List[Stmt], environment: Environment): Unit =
        val previous = this.environment
        this.environment = environment
        try
            statements.foreach: s =>
                execute(s)
        // make sure to ignore throwed Return's
        catch case e: RuntimeError => print(s"Error in block: ${e.getMessage}")
        finally this.environment = previous

    def visitIfStmt(expr: Stmt.If): Unit =
        if isTruthy(evaluate(expr.condition)) then execute(expr.thenBranch)
        else
            expr.elseBranch match
                case Some(elseBranch) => execute(elseBranch)
                case None             => ()

    def visitFunctionStmt(stmt: Stmt.Function): Unit =
        val function = LoxFunction(stmt, environment)
        environment.define(stmt.name.lexeme, function)

    def visitReturnStmt(stmt: Stmt.Return): Unit =
        val value = stmt.value match
            case Some(value) => evaluate(value)
            case None        => null
        throw Return(value)

    def visitClassStmt(stmt: Stmt.Class): Unit =
        environment.define(stmt.name.lexeme, null)
        val klass = LoxClass(stmt.name.lexeme)
        environment.assign(stmt.name, klass)

def isTruthy(value: Any): Boolean =
    value match
        case null       => false
        case b: Boolean => b
        case _          => true
