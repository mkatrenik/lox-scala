package lox

extension [T](x: T | Null)
    inline def nullToOption: Option[T] =
        Option(x).map(_.nn)

trait LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any
    def arity: Int

class LoxFunction(
    declaration: Stmt.Function,
    val closure: Environment
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any =
        val environment = Environment(Some(closure))
        for (i <- 0 until declaration.params.size) do
            environment.define(declaration.params(i).lexeme, arguments(i))
        try interpreter.executeBlock(declaration.body, environment)
        catch case r: Return => return r.value
        null

    override def arity: Int = declaration.params.size
    override def toString: String = s"<fn ${declaration.name.lexeme}>"

class Return(val value: Any) extends RuntimeException:
    // override def fillInStackTrace(): Throwable = this
    override def getMessage: String = value.toString
    override def toString: String = s"Return($value)"
