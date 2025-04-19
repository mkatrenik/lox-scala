package lox

import scala.annotation.targetName

final class AstPrinter extends ExprVisitor[String], StmtVisitor[String]:
    @targetName("toStringExpression")
    def toString(exprs: List[Expr]): String =
        exprs.map(visit).mkString(", ")

    @targetName("toStringStatement")
    def toString(stmt: List[Stmt]): String =
        stmt.map(visit).mkString(", ")

    def visit(expr: Expr): String = expr.accept(this)
    def visit(stmt: Stmt): String = stmt.accept(this)

    def visitAssignExpr(expr: Expr.Assign): String =
        s"(${expr.name.lexeme} ${visit(expr.value)})"

    def visitBinaryExpr(expr: Expr.Binary): String =
        s"(${expr.operator.lexeme} ${visit(expr.left)} ${visit(expr.right)})"

    def visitCallExpr(expr: Expr.Call): String =
        s"(${visit(expr.callee)} ${expr.arguments.map(visit).mkString(" ")})"

    def visitGetExpr(expr: Expr.Get): String =
        s"(${visit(expr.objectExpr)} ${expr.name.lexeme})"

    def visitGroupingExpr(expr: Expr.Grouping): String =
        s"(${visit(expr.expression)})"

    def visitLiteralExpr(expr: Expr.Literal): String =
        s"${expr.value}"

    def visitLogicalExpr(expr: Expr.Logical): String =
        s"(${expr.operator.lexeme} ${visit(expr.left)} ${visit(expr.right)})"

    def visitSetExpr(expr: Expr.Set): String =
        s"(${visit(expr.objectExpr)} ${expr.name.lexeme} ${visit(expr.value)})"

    def visitSuperExpr(expr: Expr.Super): String =
        s"(${expr.keyword.lexeme} ${expr.method.lexeme})"

    def visitThisExpr(expr: Expr.This): String =
        s"(${expr.keyword.lexeme})"

    def visitUnaryExpr(expr: Expr.Unary): String =
        s"(${expr.operator.lexeme} ${visit(expr.right)})"

    def visitVariableExpr(expr: Expr.Variable): String =
        s"(${expr.name.lexeme})"

    def visitExpressionStmt(stmt: Stmt.Expression): String =
        s"(${visit(stmt.expression)})"

    def visitPrintStmt(stmt: Stmt.Print): String =
        s"(print ${visit(stmt.expression)})"

    def visitVarStmt(stmt: Stmt.Var): String =
        val initializer = stmt.initializer match
            case None        => ""
            case Some(value) => s" ${visit(value)}"
        s"(var ${stmt.name.lexeme}$initializer)"

    def visitBlockStmt(stmt: Stmt.Block): String =
        val statements = stmt.statements.map(visit).mkString(" ")
        s"(block $statements)"

    def visitIfStmt(stmt: Stmt.If): String =
        val elseBranch = stmt.elseBranch match
            case None        => ""
            case Some(value) => s" ${visit(value)}"
        s"(if ${visit(stmt.condition)} ${visit(stmt.thenBranch)}$elseBranch)"

    def visitWhileStmt(stmt: Stmt.While): String =
        s"(while ${visit(stmt.condition)} ${visit(stmt.body)})"

object AstPrinter:
    def print(expr: Expr): String = AstPrinter().visit(expr)
