package lox

extension [T](x: T | Null)
    inline def nullToOption: Option[T] =
        Option(x).map(_.nn)
