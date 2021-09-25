package com.jourei.compiler

import cats.data.State
import cats.implicits.catsSyntaxOptionId
import com.jourei.compiler.OptionOps.{ >>=, flatMap }

import scala.annotation.tailrec
import scala.util.chaining.*

object Lexer:
  private type Tokens = List[Token]

  def doLexicalAnalysis(s: String): Option[Tokens] =
    @tailrec
    def go(s: Chars)(tokens: Tokens): Option[Tokens] =
      def getNumber(c: Char)(s: Chars): (Token, Chars) =
        type NumberStr = Chars
        @tailrec
        def go(acc: Chars)(float: Boolean)(
            s: Chars): (NumberStr, Boolean, Chars) =
          s match
            case c :: s if c.isDigit => go(c :: acc)(float)(s)
            case (c0 @ '.') :: c1 :: s if !float && c1.isDigit =>
              go(c1 :: c0 :: acc)(true)(s)
            case _ => (acc.reverse, float, s)

        go(List(c))(false)(s) match
          case (n, isReal, s) =>
            val numberStr = n.mkString
            (
              if isReal then Token.RealConst(numberStr.toFloat)
              else Token.IntegerConst(numberStr.toInt),
              s)
      end getNumber

      def peek(beginning: String)(s: Chars): Boolean =
        @tailrec
        def beginWith(beginning: Chars)(s: Chars): Boolean =
          (beginning, s) match
            case (Nil, _)                            => true
            case (first :: cs, c :: s) if first == c => beginWith(cs)(s)
            case _                                   => false
        beginWith(beginning.toList)(s)

      @tailrec
      def loopGetString(s: Chars)(acc: Chars): (Chars, Chars) =
        s match
          case c :: s if c.isLetterOrDigit => loopGetString(s)(c.toUpper :: acc)
          case _                           => (s, acc)

      def toID(s: String): Token = Token.ID(s)

      def getNameWith(f: String => Token)(init: Char)(
          s: Chars): (Token, Chars) =
        loopGetString(s)(List(init.toUpper)) match
          case (s, acc) => (f(acc.reverse.mkString), s)

      def getID(init: Char)(s: Chars): (Token, Chars) =
        getNameWith(toID)(init)(s)

      def getIDOrReservedKeyword(init: Char)(s: Chars): (Token, Chars) =
        def toToken(s: String): Token =
          def toKeyword(s: String): Option[Token] =
            s match
              case "BEGIN"     => Token.Begin.some
              case "END"       => Token.End.some
              case "DIV"       => Token.IntegerDiv.some
              case "PROGRAM"   => Token.Program.some
              case "PROCEDURE" => Token.Procedure.some
              case "VAR"       => Token.Var.some
              case "INTEGER"   => Token.Integer.some
              case "REAL"      => Token.Real.some
              case _           => Option.empty
          end toKeyword

          toKeyword(s).getOrElse(toID(s))
        end toToken

        getNameWith(toToken)(init)(s)
      end getIDOrReservedKeyword

      inline def getAndGoWith[A](f: A => Chars => (Token, Chars))(init: A)(
          remain: Chars): Option[Tokens] =
        f(init)(remain) match
          case (token, cs) => go(cs)(token :: tokens)

      s match
        case Nil => Some(Token.EOF :: tokens)
        case c :: s if c.isSpaceChar || c == '\n' || c == '\r' => go(s)(tokens)
        case c :: s if c.isDigit => getAndGoWith(getNumber)(c)(s)
        case '+' :: s            => go(s)(Token.Plus :: tokens)
        case '-' :: s            => go(s)(Token.Minus :: tokens)
        case '*' :: s            => go(s)(Token.Mul :: tokens)
        case '/' :: s            => go(s)(Token.FloatDiv :: tokens)
        case '(' :: s            => go(s)(Token.LParen :: tokens)
        case ')' :: s            => go(s)(Token.RParen :: tokens)
        case ':' :: s if peek("=")(s) =>
          go(s.tail)(Token.Assign :: tokens)
        case ':' :: s             => go(s)(Token.Colon :: tokens)
        case ';' :: s             => go(s)(Token.Semi :: tokens)
        case '.' :: s             => go(s)(Token.Dot :: tokens)
        case ',' :: s             => go(s)(Token.Comma :: tokens)
        case (c @ '_') :: s       => getAndGoWith(getID)(c)(s)
        case c :: s if c.isLetter => getAndGoWith(getIDOrReservedKeyword)(c)(s)
        case '{' :: s =>
          @tailrec
          def skipComment(s: Chars): Option[Chars] =
            s match
              case Nil    => Option.empty
              case c :: s => if c == '}' then Some(s) else skipComment(s)

          skipComment(s) >>= (go(_)(tokens))
        case _ => Option.empty
      end match
    end go

    go(s.toList)(List.empty).map(_.reverse)
  end doLexicalAnalysis

  private type Chars = List[Char]
end Lexer

enum Token:
  case Program, Var, Procedure, Colon, Comma, Integer, Real, Plus, Minus, Mul,
  IntegerDiv, FloatDiv, LParen, RParen, EOF, Begin, End, Dot, Assign, Semi
  case IntegerConst(v: Int)
  case RealConst(v: Float)
  case ID(v: String)
