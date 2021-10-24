package com.jourei.compiler.parser

import cats.mtl.syntax.raise.*
import cats.mtl.syntax.state.*
import cats.mtl.{ Raise, Stateful }
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.{ FlatMap, Monad }
import com.jourei.compiler.data.LocatedToken

object ParserStates:
  private def dropToken(n: Int)(tokens: Seq[LocatedToken]): Seq[LocatedToken] =
    tokens.drop(n)

  def dropTokenS[F[_]](n: Int)(using Stateful[F, Seq[LocatedToken]]): F[Unit] =
    dropToken(n).modify
