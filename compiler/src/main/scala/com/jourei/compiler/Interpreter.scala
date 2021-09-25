package com.jourei.compiler

import cats.implicits.{ catsSyntaxEitherId, catsSyntaxOptionId }
import cats.instances.either.*
import cats.instances.map.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.unorderedTraverse.*
import com.jourei.compiler.data.AST.*
import com.jourei.compiler.data.AST.Expr.{ BOp, UOp }
import com.jourei.compiler.data.Numeric.*
import com.jourei.compiler.data.{
  BuiltinType,
  BuiltinTypeSymbol,
  Numeric,
  VType,
  VarSymbol,
  VarSymbolTable
}

import scala.util.Try

object Interpreter {
  type ErrorOr[A] = Either[String, A]
  type GlobalMemory = Map[String, Numeric]
  type BuiltinTypeSymbolTable = Set[BuiltinTypeSymbol]

  inline def interpret(program: Program): ErrorOr[GlobalMemory] =
    interpretBlock(program.block)

  private inline def interpretProcedure(
      procedure: Procedure): ErrorOr[GlobalMemory] =
    interpretBlock(procedure.block)

  def interpretBlock(block: Block): ErrorOr[GlobalMemory] = {
    extension (typeDefs: TypeDefs) {
      def toVarSymbolTable(builtinTypeSymbolTable: BuiltinTypeSymbolTable)
          : ErrorOr[VarSymbolTable] = {
        def doOnce(builtinTypeSymbolTable: BuiltinTypeSymbolTable)(
            variable: Var,
            typeSpec: TypeSpec): ErrorOr[(String, VType)] = {
          def toVType(typeSpec: TypeSpec)(
              builtinTypeSymbolTable: BuiltinTypeSymbolTable)
              : ErrorOr[VType] = {
            def checkTypeSpecWithSymbolTable(typeSpec: TypeSpec)(
                builtinTypeSymbolTable: BuiltinTypeSymbolTable)
                : ErrorOr[TypeSpec] = {
              def isInSymbolTable(typeSpec: TypeSpec)(
                  builtinTypeSymbolTable: BuiltinTypeSymbolTable): Boolean = {
                inline def toBuiltinTypeSymbol(
                    typeSpec: TypeSpec): BuiltinTypeSymbol =
                  BuiltinTypeSymbol(typeSpec match
                    case TypeSpec.Real    => BuiltinType.Real
                    case TypeSpec.Integer => BuiltinType.Integer
                  )

                builtinTypeSymbolTable contains toBuiltinTypeSymbol(typeSpec)
              }

              Either.cond(
                isInSymbolTable(typeSpec)(builtinTypeSymbolTable),
                typeSpec,
                s"invalid type \"$typeSpec\"")
            }

            checkTypeSpecWithSymbolTable(typeSpec)(builtinTypeSymbolTable).map {
              case TypeSpec.Real    => VType.Builtin(BuiltinType.Real)
              case TypeSpec.Integer => VType.Builtin(BuiltinType.Integer)
            }
          }

          for vType <- toVType(typeSpec)(builtinTypeSymbolTable)
          yield (variable.toStr, vType)
        }

        typeDefs.toMap.toList
          .traverse(doOnce(builtinTypeSymbolTable))
          .map(VarSymbolTable.fromTupleSeq)
      }
    }

    inline def initBuiltinTypeTable(): BuiltinTypeSymbolTable =
      Set(
        BuiltinTypeSymbol(BuiltinType.Real),
        BuiltinTypeSymbol(BuiltinType.Integer))

    block.typedefs
      .toVarSymbolTable(initBuiltinTypeTable())
      .flatMap(interpretCompound(block.compound)(Map.empty)(_))
  }

  def interpretCompound(compound: Compound)(globalMemory: GlobalMemory)(
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
                varSymbolTable: VarSymbolTable): ErrorOr[(String, Numeric)] =
              varSymbolTable.get(id) match {
                case None => s"Variable $id has not been declared".asLeft
                case Some(VarSymbol(name, vType)) =>
                  inline def haveRightType(n: Numeric)(vType: VType): Boolean =
                    vType match {
                      case VType.Builtin(BuiltinType.Integer) => n.isInt
                      case _                                  => n.isFloat
                    }
                  inline def matchType(id: String)(n: Numeric)(
                      vType: VType): ErrorOr[(String, Numeric)] =
                    Either.cond(
                      haveRightType(n)(vType),
                      (id, n),
                      s"The type of $id is incorrect")

                  matchType(id)(n)(vType)
              }

            for {
              n <- interpret(expr)(globalMemory)(varSymbolTable)
              boundVariable <- bindVariable(variable.toStr)(n)(varSymbolTable)
            } yield globalMemory + boundVariable
          case Statement.InnerCompound(compound) =>
            interpretCompound(compound)(globalMemory)(varSymbolTable)
          case Statement.NoOp => globalMemory.asRight
        }

      statements.foldM(globalMemory)((symbolTable, statement) =>
        interpretStatement(statement)(symbolTable)(varSymbolTable))
    }

    interpretStatements(compound.v)(globalMemory)
  }
}
