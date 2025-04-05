package lox

class AstPrinter extends Visitor[String]:
    def toString(exprs: List[Expr]): String =
        exprs.map(visit).mkString(", ")

    def visit(expr: Expr): String = expr.accept(this)

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

object AstPrinter:
    def print(expr: Expr): String = AstPrinter().visit(expr)
