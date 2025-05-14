package lox
import pprint.pprintln

extension [T](x: T)
    inline def nullToOption: Option[T] =
        Option(x).map(_.nn)

extension [T](x: T)
    inline def tap: T =
        pprintln(x); x

object Nil
type LoxLiteral = Double | String | Boolean | Nil.type
type LoxAny = LoxLiteral | LoxInstance | LoxCallable | Unit

enum FunctionType:
    case None, Function, Initializer, Method

enum ClassType:
    case None, Class, Subclass

trait LoxCallable:
    def call(interpreter: Interpreter, arguments: List[LoxAny]): LoxAny
    def arity: Int

class LoxFunction(
    declaration: Stmt.Function,
    val closure: Environment,
    val isInitializer: Boolean
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[LoxAny]): LoxAny =
        // pprintln(s"call ${declaration.name.lexeme} with $arguments")
        val environment = Environment(Some(closure))
        for (i <- 0 until declaration.params.size) do
            environment.define(declaration.params(i).lexeme, arguments(i))
        try interpreter.executeBlock(declaration.body, environment)
        catch
            case r: Return =>
                if isInitializer then closure.getAt(0, "this")
                return r.value

    def bind(instance: LoxInstance): LoxFunction =
        val environment = Environment(Some(closure))
        environment.define("this", instance)
        LoxFunction(declaration, environment, isInitializer)

    override def arity: Int = declaration.params.size
    override def toString: String = s"<fn ${declaration.name.lexeme}>"

class LoxClass(
    val name: String,
    val superclass: Option[LoxClass],
    val methods: Map[String, LoxFunction]
) extends LoxCallable:
    def call(interpreter: Interpreter, arguments: List[LoxAny]): LoxAny =
        val instance = LoxInstance(this)
        val initializer = findMethod("init")
        initializer.map(_.bind(instance).call(interpreter, arguments))
        instance

    def findMethod(name: String): Option[LoxFunction] =
        methods
            .get(name)
            .orElse(
                superclass.flatMap(_.findMethod(name))
            )

    override def toString: String = s"<class $name>"
    override def arity: Int =
        methods.get("init").map(_.arity).getOrElse(0)

class LoxInstance(val klass: LoxClass):
    private val fields = collection.mutable.Map[String, LoxAny]()

    def get(name: Token): LoxAny =
        fields.get(name.lexeme) match
            case Some(value) => value
            case None =>
                klass.findMethod(name.lexeme) match
                    case Some(method) => method.bind(this)
                    case None =>
                        throw RuntimeError(
                            name,
                            s"Undefined property '${name.lexeme}'."
                        )

    def set(name: Token, value: LoxAny): Unit =
        fields.update(name.lexeme, value)

    override def toString: String = s"<instance ${klass.name}>"

class Return(val value: LoxAny) extends RuntimeException:
    // override def fillInStackTrace(): Throwable = this
    override def getMessage: String = value.toString
    override def toString: String = s"Return($value)"
