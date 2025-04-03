package lox

class AstPrinter extends Visitor[String]:
    def print(exprs: List[Expr]): String =
        exprs.map(print).mkString(", ")

    def print(expr: Expr): String = expr.accept(this)

    def visitAssignExpr(expr: Expr.Assign): String =
        s"(${expr.name.lexeme} ${print(expr.value)})"

    def visitBinaryExpr(expr: Expr.Binary): String =
        s"(${expr.operator.lexeme} ${print(expr.left)} ${print(expr.right)})"

    def visitCallExpr(expr: Expr.Call): String =
        s"(${print(expr.callee)} ${expr.arguments.map(print).mkString(" ")})"

    def visitGetExpr(expr: Expr.Get): String =
        s"(${print(expr.objectExpr)} ${expr.name.lexeme})"

    def visitGroupingExpr(expr: Expr.Grouping): String =
        s"(${print(expr.expression)})"

    def visitLiteralExpr(expr: Expr.Literal): String =
        s"${expr.value}"

    def visitLogicalExpr(expr: Expr.Logical): String =
        s"(${expr.operator.lexeme} ${print(expr.left)} ${print(expr.right)})"

    def visitSetExpr(expr: Expr.Set): String =
        s"(${print(expr.objectExpr)} ${expr.name.lexeme} ${print(expr.value)})"

    def visitSuperExpr(expr: Expr.Super): String =
        s"(${expr.keyword.lexeme} ${expr.method.lexeme})"

    def visitThisExpr(expr: Expr.This): String =
        s"(${expr.keyword.lexeme})"

    def visitUnaryExpr(expr: Expr.Unary): String =
        s"(${expr.operator.lexeme} ${print(expr.right)})"

    def visitVariableExpr(expr: Expr.Variable): String =
        s"(${expr.name.lexeme})"

object AstPrinter:
    def print(expr: Expr): String = AstPrinter().print(expr)
