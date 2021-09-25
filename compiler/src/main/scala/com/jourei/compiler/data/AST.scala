package com.jourei.compiler.data

import com.jourei.compiler.{ Interpreter, Lexer, Parser }

object AST {
  final case class Program(name: String, block: Block)
  final case class Block(
      typedefs: TypeDefs,
      procedures: Procedures,
      compound: Compound)

  final case class Procedure(name: String, block: Block)

  opaque type Procedures = List[Procedure]
  object Procedures {
    def apply(v: List[Procedure]): Procedures = v
    def empty: Procedures = List.empty
  }

  opaque type TypeDefs = Map[Var, TypeSpec]
  object TypeDefs:
    def apply(x: Map[Var, TypeSpec]): TypeDefs = x
    def empty: TypeDefs = Map.empty
    extension (x: TypeDefs)
      def toMap: Map[Var, TypeSpec] = x
      def ++(y: TypeDefs): TypeDefs = x ++ y

  opaque type Compound = Seq[Statement]
  object Compound:
    def apply(statements: Seq[Statement]): Compound = statements
    extension (x: Compound) def v: Seq[Statement] = x

  enum Statement:
    case Assign(variable: Var, expr: Expr)
    case InnerCompound(compound: Compound)
    case NoOp

  opaque type Var = String
  object Var:
    def apply(s: String): Var = s

    extension (x: Var) def toStr: String = x

  opaque type TypeSpecs = Seq[TypeSpec]
  object TypeSpecs:
    def apply(v: Seq[TypeSpec]): TypeSpecs = v
    def empty: TypeSpecs = Seq.empty

    implicit class Ops(x: TypeSpecs) extends AnyVal:
      def +:(y: TypeSpec): TypeSpecs = x prepend y
      infix def prepend(y: TypeSpec): TypeSpecs = x.prepended(y)

  enum TypeSpec:
    case Integer, Real

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
      """
        |PROGRAM Part12;
        |VAR
        |   a : INTEGER;
        |
        |PROCEDURE P1;
        |VAR
        |   a : REAL;
        |   k : INTEGER;
        |
        |   PROCEDURE P2;
        |   VAR
        |      a, z : INTEGER;
        |   BEGIN {P2}
        |      z := 777;
        |   END;  {P2}
        |
        |BEGIN {P1}
        |
        |END;  {P1}
        |
        |BEGIN {Part12}
        |   a := 10;
        |END.  {Part12}
        |""".stripMargin
    val tokens = Lexer.doLexicalAnalysis(str).tap(println)
    val ast = Parser.parse(tokens.get).tap(println)
    val symbolTable =
      Interpreter.interpret(ast.toOption.get).tap(println)
}
