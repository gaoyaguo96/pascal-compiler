package com.jourei.compiler.data

import cats.data.{EitherT, State}
import com.jourei.compiler.LexerStates.Position
import com.jourei.compiler.data.error.{LexerError, ParserError}
import com.jourei.compiler.{Interpreter, Lexer, SemanticAnalyzer}
import com.jourei.compiler.parser.Parser

import scala.deriving.Mirror

object AST {
  final case class Program(name: String, block: Block)
  final case class Block(
      varDecls: Seq[VarDecl],
      procedures: Seq[Procedure],
      compound: Compound)

  final case class VarDecl(name: String, typeSpec: TypeSpec)

  final case class FormalParam(name: String, typeSpec: TypeSpec)

  final case class Procedure(
      name: String,
      formalParams: Seq[FormalParam],
      block: Block)

  final case class Compound(statements: Seq[Statement])

  enum Statement:
    case Assign(variable: ID, expr: Expr)
    case InnerCompound(compound: Compound)
    case NoOp

  final case class ID(name: String) extends AnyVal

  final case class TypeSpec(name: String) extends AnyVal

  enum Expr:
    case Integer(v: Int)
    case Real(v: Float)
    case Binary(op: Expr.BOp, lChild: Expr, rChild: Expr)
    case Unary(op: Expr.UOp, child: Expr)
    case Variable(v: String)

  object Expr:
    enum BOp:
      case Plus, Minus, Mul, IntegerDiv, FloatDiv
    enum UOp:
      case Plus, Minus

  import scala.util.chaining.*
  @main def hello(): Unit =
//    val separator = sys.props("line.separator").tap(println)
    val str =
      """PROGRAM Part10;
        |VAR
        |   number     : INTEGER;
        |   a, b, c, x : INTEGER;
        |   y          : REAL;
        |
        |BEGIN {Part10}
        |   BEGIN
        |      number := 2;
        |      a := number;
        |      b := 10 * a + 10 * number DIV 4;
        |      c := a - --- b
        |   END;
        |   x := 11;
        |   y := 20 / 7 + 3.14;
        |   { writeln('a = ', a); }
        |   { writeln('b = ', b); }
        |   { writeln('c = ', c); }
        |   { writeln('number = ', number); }
        |   { writeln('x = ', x); }
        |   { writeln('y = ', y); }
        |END.  {Part10}""".stripMargin
    val tokens = Lexer.doLexicalAnalysis[[b]=>>
      EitherT[[a]=>>State[Position, a], LexerError, b]](str).tap(_.value.run(Position(1,1)).value.pipe(println))

    val ast = Parser.parse[[a]=>>Either[ParserError,a]](tokens.getOrElse(List()).runA(Position(1,1)).value).tap(println)
    println(ast)
//    val ast1 = SemanticAnalyzer.analyzeProgram(ast.toOption.get).tap(println)
//    val symbolTable =
//      Interpreter.interpret(ast.toOption.get).tap(println)
}
