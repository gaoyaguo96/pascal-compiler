package com.jourei.compiler.data

import cats.syntax.either.*

import scala.util.Try

type Numeric = Int | Float

object Numeric:
  type ErrorOr[a] = Either[String, a]

  extension (x: Numeric)
    infix def plus(y: Numeric): Numeric =
      (x, y) match
        case (x: Int, y: Int)     => x + y
        case (x: Float, y: Int)   => x + y
        case (x: Int, y: Float)   => x + y
        case (x: Float, y: Float) => x + y

    infix def minus(y: Numeric): Numeric =
      (x, y) match
        case (x: Int, y: Int)     => x - y
        case (x: Float, y: Int)   => x - y
        case (x: Int, y: Float)   => x - y
        case (x: Float, y: Float) => x - y

    infix def mul(y: Numeric): Numeric =
      (x, y) match
        case (x: Int, y: Int)     => x * y
        case (x: Float, y: Int)   => x * y
        case (x: Int, y: Float)   => x * y
        case (x: Float, y: Float) => x * y

    infix def div(y: Numeric): ErrorOr[Numeric] =
      (x, y) match
        case (x: Int, y: Int) =>
          Try(x / y).toOption.toRight("Divisor must not be 0")
        case _ => "at least 1 operands isn't integers".asLeft

    infix def floatDiv(y: Numeric): Option[Float] =
      Try {
        (x, y) match
          case (x: Int, y: Int)     => x.toFloat / y
          case (x: Float, y: Int)   => x / y
          case (x: Int, y: Float)   => x / y
          case (x: Float, y: Float) => x / y
      }.toOption

    infix def opposite: Numeric =
      x match
        case x: Int   => -x
        case x: Float => -x

    inline def isInt: Boolean =
      x match
        case _: Int => true
        case _      => false

    inline def isFloat: Boolean = !isInt
  end extension
end Numeric
