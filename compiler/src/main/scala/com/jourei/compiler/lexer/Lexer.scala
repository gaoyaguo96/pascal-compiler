package com.jourei.compiler.lexer

import cats.*
import cats.data.State
import cats.mtl.syntax.raise.*
import cats.mtl.syntax.state.*
import cats.mtl.{ Raise, Stateful }
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import com.jourei.compiler.data.Token.{ toIDToken, toKeywordTokenOrIDToken }
import com.jourei.compiler.data.error.{ LexerError, PositionInSource }
import com.jourei.compiler.data.{ LocatedToken, Token }
import com.jourei.compiler.lexer.LexerStates.Position
import com.jourei.compiler.syntax.OptionOps.{ >>=, flatMap }

import scala.annotation.tailrec
import scala.util.chaining.*

object Lexer:
  private type Tokens = List[Token]
  private type LocatedTokens = Seq[LocatedToken]

  type StatefulPosition[F[_]] = Stateful[F, Position]

  def doLexicalAnalysis[F[_]: Monad: StatefulPosition](s: String)(
      using Raise[F, LexerError]): F[LocatedTokens] =
    // TODO tailrec
    def go[F[_]: Monad: StatefulPosition](
        s: Chars,
        locatedTokens: LocatedTokens)(
        using Raise[F, LexerError]): F[LocatedTokens] =
      inline def getNumber(inline c: Char, inline s: Chars): F[(Token, Chars)] =
        type NumberStr = Chars

        // TODO tailrec
        def go[F[_]: StatefulPosition: Applicative](
            acc: Chars,
            float: Boolean,
            s: Chars): F[(NumberStr, Boolean, Chars)] =
          s match
            case c :: s if c.isDigit =>
              LexerStates.processS(1) *> go(c :: acc, float, s)
            case (c0 @ '.') :: c1 :: s if !float && c1.isDigit =>
              LexerStates.processS(2) *> go(c1 :: c0 :: acc, true, s)
            case _ => (acc.reverse, float, s).pure

        for
          (n, isReal, s) <- go(List(c), false, s)
          numberStr = n.mkString
        yield (
          if isReal then Token.RealConst(numberStr.toFloat)
          else Token.IntegerConst(numberStr.toInt),
          s)
      end getNumber

      inline def peek(inline beginning: String)(inline s: Chars): Boolean =
        @tailrec
        def beginWith(beginning: Chars)(s: Chars): Boolean =
          (beginning, s) match
            case (Nil, _)                            => true
            case (first :: cs, c :: s) if first == c => beginWith(cs)(s)
            case _                                   => false

        beginWith(beginning.toList)(s)
      end peek

//      @tailrec
      def loopGetWord[F[_]: Monad: StatefulPosition](
          s: Chars,
          acc: Chars): F[(Chars, Chars)] =
        s match
          case c :: s if c.isLetterOrDigit =>
            // TODO build a tail recursion
            LexerStates.processS(1) *> loopGetWord(s, c.toUpper :: acc)
          case _ => LexerStates.processS(1) *> (acc, s).pure

      inline def getWordWith[F[_]: Monad: StatefulPosition](
          inline f: String => Token)(
          inline init: Char,
          inline s: Chars): F[(Token, Chars)] =
        loopGetWord(s, List(init.toUpper))
          .map(_.leftMap(_.reverse.mkString.pipe(f)))

      inline def getID[F[_]: Monad: StatefulPosition](
          inline init: Char,
          inline s: Chars): F[(Token, Chars)] =
        getWordWith(_.toIDToken)(init, s)

      inline def getIDOrReservedKeyword[F[_]: Monad: StatefulPosition](
          inline init: Char,
          inline s: Chars): F[(Token, Chars)] =
        getWordWith(_.toKeywordTokenOrIDToken)(init, s)

      inline def getAndGo[F[_]: Monad: StatefulPosition, A](
          inline f: (A, Chars) => F[(Token, Chars)])(lineNo: Int, column: Int)(
          inline init: A,
          inline remain: Chars)(locatedTokens: LocatedTokens)(
          using Raise[F, LexerError]): F[LocatedTokens] =
        for
          (token, cs) <- f(init, remain)
          locatedTokens <- go(
            cs,
            LocatedToken(
              token,
              PositionInSource(lineNo, column)) +: locatedTokens)
        yield locatedTokens

      inline def buildLocatedToken(
          inline token: Token,
          inline lineNo: Int,
          inline column: Int): LocatedToken =
        LocatedToken(token, PositionInSource(lineNo, column))

      for {
        Position(lineNo, column) <- Stateful.get
        newLocatedTokens <-
          s match {
            case Nil =>
              (LocatedToken(
                Token.EOF,
                PositionInSource(lineNo, column)) +: locatedTokens).pure
            case c :: s if c == '\n' || c == '\r' =>
              LexerStates.wrapS *> go(
                if peek("\n")(s) then s.tail else s,
                locatedTokens)
            case c :: s if c.isSpaceChar =>
              LexerStates.processS(1) *> go(s, locatedTokens)
            case c :: s if c.isDigit =>
              getAndGo(getNumber(_, _))(lineNo, column)(c, s)(locatedTokens)
            case '+' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Plus, lineNo, column) +: locatedTokens)
            case '-' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Minus, lineNo, column) +: locatedTokens)
            case '*' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Mul, lineNo, column) +: locatedTokens)
            case '/' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(
                  Token.FloatDiv,
                  lineNo,
                  column) +: locatedTokens)
            case '(' :: s =>
              LexerStates.processS(1) *>
                go(
                  s,
                  buildLocatedToken(
                    Token.LParen,
                    lineNo,
                    column) +: locatedTokens)
            case ')' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(
                  Token.RParen,
                  lineNo,
                  column) +: locatedTokens)
            case ':' :: s if peek("=")(s) =>
              LexerStates.processS(2) *> go(
                s.tail,
                buildLocatedToken(
                  Token.Assign,
                  lineNo,
                  column) +: locatedTokens)
            case ':' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Colon, lineNo, column) +: locatedTokens)
            case ';' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Semi, lineNo, column) +: locatedTokens)
            case '.' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Dot, lineNo, column) +: locatedTokens)
            case ',' :: s =>
              LexerStates.processS(1) *> go(
                s,
                buildLocatedToken(Token.Comma, lineNo, column) +: locatedTokens)
            case (c @ '_') :: s =>
              getAndGo(getID(_, _))(lineNo, column)(c, s)(locatedTokens)
            case c :: s if c.isLetter =>
              getAndGo(getIDOrReservedKeyword(_, _))(lineNo, column)(c, s)(
                locatedTokens)
            case '{' :: s =>
//              @tailrec
              def skipComment[F[_]: Monad: StatefulPosition](s: Chars)(
                  using Raise[F, LexerError]): F[Chars] =
                LexerStates.processS(1) *> (if s.isEmpty then
                                              Stateful.get.flatMap {
                                                case Position(lineNo, column) =>
                                                  LexerError(
                                                    "}",
                                                    PositionInSource(
                                                      lineNo,
                                                      column)).raise
                                              }
                                            else if '}' == s.head then
                                              s.tail.pure
                                            else skipComment(s.tail))

              skipComment(s) >>= (go(_, locatedTokens))
            case c :: _ =>
              LexerError(s"$c", PositionInSource(lineNo, column)).raise
          }
      } yield newLocatedTokens
    end go

    go(s.toList, List.empty).map(_.reverse)
  end doLexicalAnalysis

  private type Chars = List[Char]
end Lexer

object LexerStates:

  final case class Position(lineNo: Int, column: Int)

  private def wrap(s: Position): Position = Position(s.lineNo + 1, 1)
  private def process(length: Int)(s: Position): Position =
    if length < 0 then throw IllegalArgumentException()
    s.copy(column = s.column + length)

  def wrapS[F[_]](using Stateful[F, Position]): F[Unit] = wrap.modify
  def processS[F[_]](step: Int)(using Stateful[F, Position]): F[Unit] =
    process(step).modify

  final case class Tokens(v: Seq[Token]) extends AnyVal

  private def prependToken(token: Token)(s: Tokens): Tokens =
    Tokens(s.v.prepended(token))

  private def prependAllTokens(tokens: Token*)(s: Tokens): Tokens =
    Tokens(s.v.prependedAll(tokens))

  def prependTokenS[F[_]](token: Token)(using Stateful[F, Tokens]): F[Unit] =
    prependToken(token).modify

  def prependAllTokensS[F[_]](tokens: Token*)(
      using Stateful[F, Tokens]): F[Unit] =
    prependAllTokens(tokens*).modify
