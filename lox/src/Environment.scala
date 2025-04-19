package lox

class Environment(enclosing: Option[Environment] = None):
    private val values = scala.collection.mutable.Map[String, Any]()
    // private val locals = scala.collection.mutable.Map[Expr, Int]()

    def define(name: String, value: Any): Unit =
        values(name) = value

    def get(name: Token): Any =
        if values.contains(name.lexeme) then values(name.lexeme)
        else
            enclosing match
                case Some(env) => env.get(name)
                case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")

    def assign(name: Token, value: Any): Unit =
        if values.contains(name.lexeme) then values(name.lexeme) = value
        else
            enclosing match
                case Some(env) => env.assign(name, value)
                case None      => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
