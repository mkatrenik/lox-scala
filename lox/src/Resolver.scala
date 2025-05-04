package lox

import collection.mutable.{Map, Stack}
import scala.util.boundary, boundary.break

final class Resolver(interpreter: Interpreter) extends ExprVisitor[Unit], StmtVisitor[Unit]:
    private var currentFunction = FunctionType.None
    private var currentClass = ClassType.None
    val scopes = Stack[Map[String, Boolean]]()

    def resolve(expr: Expr): Unit =
        expr.accept(this)

    def resolve(stmt: Stmt): Unit =
        stmt.accept(this)

    def beginScope(): Unit =
        scopes.push(Map[String, Boolean]())

    def endScope(): Unit =
        if scopes.isEmpty then throw new RuntimeException("No scope to end.")
        scopes.pop()
        ()

    def resolve(statements: List[Stmt]): Unit =
        for (statement <- statements) do resolve(statement)

    def visitBlockStmt(stmt: Stmt.Block): Unit =
        beginScope()
        resolve(stmt.statements)
        endScope()

    def visitVarStmt(stmt: Stmt.Var): Unit =
        declare(stmt.name)
        stmt.initializer.map(resolve(_))
        define(stmt.name)

    def declare(name: Token): Unit =
        if scopes.isEmpty then return
        val scope = scopes.top
        if scope.contains(name.lexeme) then
            Lox.error(name, "Already a variable with this name in this scope.")
        scope.addOne(name.lexeme -> false)
        ()

    def define(name: Token): Unit =
        if scopes.isEmpty then return
        val scope = scopes.top
        scope.update(name.lexeme, true)

    def visitVariableExpr(expr: Expr.Variable): Unit =
        if !scopes.isEmpty && scopes.top.get(expr.name.lexeme).contains(false) then
            Lox.error(expr.name, "Cannot read local variable in its own initializer.")
        resolveLocal(expr, expr.name)

    def resolveLocal(expr: Expr, name: Token): Unit =
        boundary:
            for (i <- scopes.indices) do
                if scopes(i).contains(name.lexeme) then
                    interpreter.resolve(expr, i)
                    break()

    def visitExpressionStmt(stmt: Stmt.Expression): Unit =
        resolve(stmt.expression)

    def visitIfStmt(stmt: Stmt.If): Unit =
        resolve(stmt.condition)
        resolve(stmt.thenBranch)
        stmt.elseBranch.foreach(resolve(_))

    def visitWhileStmt(stmt: Stmt.While): Unit =
        resolve(stmt.condition)
        resolve(stmt.body)

    def visitPrintStmt(stmt: Stmt.Print): Unit =
        resolve(stmt.expression)

    def visitReturnStmt(stmt: Stmt.Return): Unit =
        if currentFunction == FunctionType.None then
            Lox.error(stmt.keyword, "Cannot return from top-level code.")
        else if currentFunction == FunctionType.Initializer then
            Lox.error(stmt.keyword, "Cannot return a value from an initializer.")
        else stmt.value.foreach(resolve(_))

    def visitBinaryExpr(expr: Expr.Binary): Unit =
        resolve(expr.left)
        resolve(expr.right)

    def visitCallExpr(expr: Expr.Call): Unit =
        resolve(expr.callee)
        for (argument <- expr.arguments) do resolve(argument)

    def visitGroupingExpr(expr: Expr.Grouping): Unit =
        resolve(expr.expression)

    def visitLiteralExpr(expr: Expr.Literal): Unit =
        ()

    def visitLogicalExpr(expr: Expr.Logical): Unit =
        resolve(expr.left)
        resolve(expr.right)

    def visitUnaryExpr(expr: Expr.Unary): Unit =
        resolve(expr.right)

    def visitFunctionStmt(stmt: Stmt.Function): Unit =
        declare(stmt.name)
        define(stmt.name)
        resolveFunction(stmt, FunctionType.Function)

    def resolveFunction(stmt: Stmt.Function, `type`: FunctionType): Unit =
        val enclosingFunction = currentFunction
        currentFunction = `type`
        beginScope()
        for (param <- stmt.params) do
            declare(param)
            define(param)
        resolve(stmt.body)
        endScope()
        currentFunction = enclosingFunction

    def visitAssignExpr(expr: Expr.Assign): Unit =
        resolve(expr.value)
        resolveLocal(expr, expr.name)

    def visitClassStmt(stmt: Stmt.Class): Unit =
        val enclosingClass = currentClass
        currentClass = ClassType.Class

        declare(stmt.name)
        define(stmt.name)

        stmt.superclass.foreach: superclass =>
            if stmt.name.lexeme == superclass.name.lexeme then
                Lox.error(superclass.name, "A class cannot inherit from itself.")

            currentClass = ClassType.Subclass
            resolve(superclass)

            beginScope()
            scopes.top.addOne("super" -> true)

        beginScope()
        scopes.top.addOne("this" -> true)

        stmt.methods.foreach: method =>
            val declaration =
                if method.name.lexeme == "init" then FunctionType.Initializer
                else FunctionType.Method
            resolveFunction(method, declaration)

        endScope()
        if stmt.superclass.isDefined then endScope()
        currentClass = enclosingClass

    def visitGetExpr(expr: Expr.Get): Unit =
        resolve(expr.objectExpr)

    def visitSetExpr(expr: Expr.Set): Unit =
        resolve(expr.value)
        resolve(expr.objectExpr)

    def visitThisExpr(expr: Expr.This): Unit =
        if currentClass == ClassType.None then
            Lox.error(expr.keyword, "Cannot use 'this' outside of a class.")
        else resolveLocal(expr, expr.keyword)

    def visitSuperExpr(expr: Expr.Super): Unit =
        if currentClass == ClassType.None then
            Lox.error(expr.keyword, "Cannot use 'super' outside of a class.")
        else if currentClass != ClassType.Subclass then
            Lox.error(expr.keyword, "Cannot use 'super' in a class with no superclass.")
        else resolveLocal(expr, expr.keyword)
