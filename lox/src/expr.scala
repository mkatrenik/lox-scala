package lox

import java.util.UUID

enum Expr:
    case Assign(name: Token, value: Expr)
    case Binary(left: Expr, operator: Token, right: Expr)
    case Call(callee: Expr, paren: Token, arguments: List[Expr])
    case Get(objectExpr: Expr, name: Token)
    case Grouping(expression: Expr)
    case Literal(value: Any)
    case Logical(left: Expr, operator: Token, right: Expr)
    case Set(objectExpr: Expr, name: Token, value: Expr)
    case Super(keyword: Token, method: Token)
    case This(keyword: Token)
    case Unary(operator: Token, right: Expr)
    case Variable(name: Token)

    // used in cases where we need to track individual expressions
    val uniqueId: UUID = UUID.randomUUID()

    def accept[T](visitor: ExprVisitor[T]): T =
        this match
            case a @ Assign(_, _)     => visitor.visitAssignExpr(a)
            case b @ Binary(_, _, _)  => visitor.visitBinaryExpr(b)
            case c @ Call(_, _, _)    => visitor.visitCallExpr(c)
            case g @ Get(_, _)        => visitor.visitGetExpr(g)
            case g @ Grouping(_)      => visitor.visitGroupingExpr(g)
            case l @ Literal(_)       => visitor.visitLiteralExpr(l)
            case l @ Logical(_, _, _) => visitor.visitLogicalExpr(l)
            case s @ Set(_, _, _)     => visitor.visitSetExpr(s)
            case s @ Super(_, _)      => visitor.visitSuperExpr(s)
            case t @ This(_)          => visitor.visitThisExpr(t)
            case u @ Unary(_, _)      => visitor.visitUnaryExpr(u)
            case v @ Variable(_)      => visitor.visitVariableExpr(v)
