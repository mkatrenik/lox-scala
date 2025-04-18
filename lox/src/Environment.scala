package lox

class Environment():
    private val values = scala.collection.mutable.Map[String, Any]()
    // private val locals = scala.collection.mutable.Map[Expr, Int]()

    def define(name: String, value: Any): Unit =
        values(name) = value

    def get(name: Token): Any =
        values.getOrElse(name.lexeme, throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))

    def assign(name: Token, value: Any): Unit =
        if values.contains(name.lexeme) then values(name.lexeme) = value
        else throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
