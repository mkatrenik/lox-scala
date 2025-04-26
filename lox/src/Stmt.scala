package lox

enum Stmt:
    case Block(statements: List[Stmt])
    // case Class(name: Token, superClass: Option[VariableExpr], methods: List<FunctionStmt>)
    case Expression(expression: Expr)
    case Function(name: Token, params: List[Token], body: List[Stmt])
    case If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])
    case Print(expression: Expr)
    case Return(keyword: Token, value: Option[Expr])
    case Var(name: Token, initializer: Option[Expr])
    case While(condition: Expr, body: Stmt)
    case For(initializer: Option[Stmt], condition: Option[Expr], increment: Option[Expr], body: Stmt)
    // case Import(name: Token, alias: Option[Token])
    // case ImportAll(name: Token, alias: Option[Token])
    // case ImportDefault(name: Token, alias: Option[Token])

    def accept[T](visitor: StmtVisitor[T]): T =
        this match
            case b @ Block(_) => visitor.visitBlockStmt(b)
            // case c @ Class(_, _, _)    => visitor.visitClassStmt(c)
            case e @ Expression(_)     => visitor.visitExpressionStmt(e)
            case f @ Function(_, _, _) => visitor.visitFunctionStmt(f)
            case i @ If(_, _, _)       => visitor.visitIfStmt(i)
            case p @ Print(_)          => visitor.visitPrintStmt(p)
            case r @ Return(_, _)      => visitor.visitReturnStmt(r)
            case v @ Var(_, _)         => visitor.visitVarStmt(v)
            case w @ While(_, _)       => visitor.visitWhileStmt(w)
            case f @ For(_, _, _, _)   => visitor.visitForStmt(f)
            // case i @ Import(_, _)      => visitor.visitImportStmt(i)
            // case i @ ImportAll(_, _)   => visitor.visitImportAllStmt(i)
            // case i @ ImportDefault(_, _) =>
            //     visitor.visitImportDefaultStmt(i)
