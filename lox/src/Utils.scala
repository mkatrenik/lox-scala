package lox

extension [T](x: T | Null)
    inline def nullToOption: Option[T] =
        Option(x).map(_.nn)

trait LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any
    def arity: Int

class LoxFunction(
    declaration: Stmt.Function
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any =
        val environment = Environment(Some(interpreter.globals))
        for (i <- 0 until declaration.params.size) do environment.define(declaration.params(i).lexeme, arguments(i))
        interpreter.executeBlock(declaration.body, environment)

    def arity: Int = declaration.params.size

    override def toString: String = s"<fn ${declaration.name.lexeme}>"
