package com.jourei.compiler.data

import cats.data.{EitherT, State}
import com.jourei.compiler.lexer.LexerStates.Position
import com.jourei.compiler.data.error.{LexerError, ParserError}
import com.jourei.compiler.parser.Parser
import com.jourei.compiler.semantic.SemanticAnalyzer

import scala.deriving.Mirror

object AST:
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
end AST
