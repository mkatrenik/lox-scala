package lox
import pprint.pprintln
import java.util.concurrent.atomic.AtomicInteger

val counter = new AtomicInteger(0)

extension [T](x: T | Null)
    inline def nullToOption: Option[T] =
        Option(x).map(_.nn)

extension [T](x: T)
    inline def tap: T =
        pprintln(x); x

enum FunctionType:
    case None, Function, Initializer, Method

enum ClassType:
    case None, Class

trait LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any
    def arity: Int

class LoxFunction(
    declaration: Stmt.Function,
    val closure: Environment,
    val isInitializer: Boolean
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any =
        // pprintln(s"call ${declaration.name.lexeme} with $arguments")
        val environment = Environment(Some(closure))
        for (i <- 0 until declaration.params.size) do
            environment.define(declaration.params(i).lexeme, arguments(i))
        try interpreter.executeBlock(declaration.body, environment)
        catch
            case r: Return =>
                if isInitializer then closure.getAt(0, "this")
                return r.value
        null

    def bind(instance: LoxInstance): LoxFunction =
        val environment = Environment(Some(closure))
        environment.define("this", instance)
        LoxFunction(declaration, environment, isInitializer)

    override def arity: Int = declaration.params.size
    override def toString: String = s"<fn ${declaration.name.lexeme}>"

class LoxClass(
    val name: String,
    // val superclass: Option[LoxClass],
    val methods: Map[String, LoxFunction]
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[Any]): Any =
        val instance = LoxInstance(this)
        val initializer = findMethod("init")
        initializer.map(_.bind(instance).call(interpreter, arguments))
        instance

    private def findMethod(name: String): Option[LoxFunction] =
        methods.get(name)

    override def toString: String = s"<class $name>"
    override def arity: Int =
        methods.get("init").map(_.arity).getOrElse(0)

class LoxInstance(val klass: LoxClass):
    private val fields = collection.mutable.Map[String, Any]()

    def get(name: Token): Any =
        fields.get(name.lexeme) match
            case Some(value) => value
            case None =>
                klass.methods.get(name.lexeme) match
                    case Some(method) => method.bind(this)
                    case None =>
                        throw RuntimeError(
                            name,
                            s"Undefined property '${name.lexeme}'."
                        )

    def set(name: Token, value: Any): Unit =
        fields.update(name.lexeme, value)

    override def toString: String = s"<instance ${klass.name}>"

class Return(val value: Any) extends RuntimeException:
    // override def fillInStackTrace(): Throwable = this
    override def getMessage: String = value.toString
    override def toString: String = s"Return($value)"
