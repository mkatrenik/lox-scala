package lox

trait Visitor[R]:
    def visitAssignExpr(expr: Expr.Assign): R
    def visitBinaryExpr(expr: Expr.Binary): R
    def visitCallExpr(expr: Expr.Call): R
    def visitGetExpr(expr: Expr.Get): R
    def visitGroupingExpr(expr: Expr.Grouping): R
    def visitLiteralExpr(expr: Expr.Literal): R
    def visitLogicalExpr(expr: Expr.Logical): R
    def visitSetExpr(expr: Expr.Set): R
    def visitSuperExpr(expr: Expr.Super): R
    def visitThisExpr(expr: Expr.This): R
    def visitUnaryExpr(expr: Expr.Unary): R
    def visitVariableExpr(expr: Expr.Variable): R
