package com.jourei.compiler

import cats.implicits.{ catsSyntaxEitherId, catsSyntaxOptionId }
import cats.instances.either.*
import cats.instances.map.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.jourei.compiler.data.LStack
import com.jourei.compiler.data.AST.*
import com.jourei.compiler.data.AST.Expr.{ BOp, UOp }
import com.jourei.compiler.data.Numeric.*
import com.jourei.compiler.data.{
  BuiltinTypeSymbol,
  Numeric,
  ScopedSymbolTable,
  VarSymbol,
  VarSymbolTable
}

import scala.util.Try
import scala.util.chaining.*

object Interpreter {
  type ErrorOr[A] = Either[String, A]
  type GlobalMemory = Map[String, Numeric]

  inline def interpret(program: Program): ErrorOr[GlobalMemory] = ???

//  private inline def interpretProcedure(procedure: Procedure)(
//      varSymbolTableStack: LStack[VarSymbolTable]): ErrorOr[GlobalMemory] =
//    for {
//      uppermostScope <- varSymbolTableStack.headOption.toRight(
//        "There is no scope")
//
//    } yield interpretBlock(procedure.name)(procedure.block)

  type VarSymbolTableStack = LStack[VarSymbolTable]
  private def interpretBlock(name: String)(
      block: Block)(varSymbolTableStack:LStack[VarSymbolTable]): ErrorOr[GlobalMemory] = ???

  private def interpretCompound(compound: Compound)(globalMemory: GlobalMemory)(
      varSymbolTable: VarSymbolTable): ErrorOr[GlobalMemory] = {
    def interpretStatements(statements: Seq[Statement])(
        globalMemory: GlobalMemory): ErrorOr[GlobalMemory] = {
      def interpretStatement(statement: Statement)(globalMemory: GlobalMemory)(
          varSymbolTable: VarSymbolTable): ErrorOr[GlobalMemory] =
        statement match {
          case Statement.Assign(variable, expr) =>
            def interpret(expr: Expr)(globalMemory: GlobalMemory)(
                varSymbolTable: VarSymbolTable): ErrorOr[Numeric] =
              expr match {
                case Expr.Integer(v) => v.asRight
                case Expr.Real(v)    => v.asRight
                case Expr.Binary(op, lChild, rChild) =>
                  extension (astOp: Expr.BOp) {
                    inline def toOp(x: Numeric)(y: Numeric): ErrorOr[Numeric] =
                      astOp match {
                        case BOp.Plus       => (x plus y).asRight
                        case BOp.Minus      => (x minus y).asRight
                        case BOp.Mul        => (x mul y).asRight
                        case BOp.IntegerDiv => x div y
                        case BOp.FloatDiv =>
                          (x floatDiv y).toRight("The divisor must not be 0")
                      }
                  }

                  for {
                    x <- interpret(lChild)(globalMemory)(varSymbolTable)
                    y <- interpret(rChild)(globalMemory)(varSymbolTable)
                    n <- op.toOp(x)(y)
                  } yield n
                case Expr.Unary(op, child) =>
                  inline def astOpToOp(astOp: => Expr.UOp): Numeric => Numeric =
                    astOp match {
                      case UOp.Plus  => identity
                      case UOp.Minus => _.opposite
                    }

                  for n <- interpret(child)(globalMemory)(varSymbolTable)
                  yield astOpToOp(op)(n)
                case Expr.Variable(v) =>
                  globalMemory.get(v).toRight(s"$v is an undefined variable")
              }

            def bindVariable(id: String)(n: Numeric)(
                varSymbolTable: VarSymbolTable): ErrorOr[(String, Numeric)] = ???
//              varSymbolTable.get(id) match {
//                case None => s"Variable $id has not been declared".asLeft
//                case Some(VarSymbol(name, vType)) =>
//                  inline def haveRightType(n: Numeric)(vType: String): Boolean =
////                    vType match {
////                      case "INTEGER" => n.isInt
////                      case "REAL"                                        => n.isFloat
////                      case _ => false
////                    }
//                    ???
//
//                  inline def matchType(id: String)(n: Numeric)(
//                      vType: String): ErrorOr[(String, Numeric)] =
//                    Either.cond(
//                      haveRightType(n)(vType),
//                      (id, n),
//                      s"The type of $id is incorrect")
//
//                  matchType(id)(n)(vType)
//              }

            for {
              n <- interpret(expr)(globalMemory)(varSymbolTable)
              boundVariable <- bindVariable(variable.name)(n)(varSymbolTable)
            } yield globalMemory + boundVariable
          case Statement.InnerCompound(compound) =>
            interpretCompound(compound)(globalMemory)(varSymbolTable)
          case Statement.NoOp => globalMemory.asRight
        }

      statements.foldM(globalMemory)((symbolTable, statement) =>
        interpretStatement(statement)(symbolTable)(varSymbolTable))
    }

    interpretStatements(compound.statements)(globalMemory)
  }
}
