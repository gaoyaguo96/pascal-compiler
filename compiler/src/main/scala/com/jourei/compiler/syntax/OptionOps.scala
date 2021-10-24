package com.jourei.compiler.syntax

object OptionOps:
  extension [A](fa: Option[A])
    inline def >>=[B](inline f: A => Option[B]): Option[B] =
      if fa.isEmpty then None else f(fa.get)

    inline def flatMap[B](inline f: A => Option[B]): Option[B] = fa >>= f
