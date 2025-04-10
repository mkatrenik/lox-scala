package lox

trait ExprVisitor[R]:
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

trait StmtVisitor[R]:
    // def visitBlockStmt(stmt: Stmt.Block): R
    // def visitClassStmt(stmt: Stmt.Class): R
    def visitExpressionStmt(stmt: Stmt.Expression): R
    // def visitFunctionStmt(stmt: Stmt.Function): R
    // def visitIfStmt(stmt: Stmt.If): R
    def visitPrintStmt(stmt: Stmt.Print): R
    // def visitReturnStmt(stmt: Stmt.Return): R
    // def visitVarStmt(stmt: Stmt.Var): R
    // def visitWhileStmt(stmt: Stmt.While): R
    // def visitImportStmt(stmt: Stmt.Import): R
    // def visitImportAllStmt(stmt: Stmt.ImportAll): R
    // def visitImportDefaultStmt(stmt: Stmt.ImportDefault): R
