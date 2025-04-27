package lox

final class Environment(val enclosing: Option[Environment] = None):
    private val values = scala.collection.mutable.Map[String, Any]()

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

    def assignAt(distance: Int, name: Token, value: Any): Unit =
        ancestor(distance).values.put(name.lexeme, value)

    def getAt(distance: Int, name: String): Any =
        ancestor(distance).values(name)

    def ancestor(distance: Int): Environment =
        var env = this
        for _ <- 0 until distance do env = env.enclosing.get
        env
