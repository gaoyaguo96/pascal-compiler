package com.jourei.compiler.semantic

import cats.data.{ Kleisli, ReaderT, WriterT }
import cats.mtl.Raise
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.semigroupk.*
import cats.{ Applicative, ApplicativeError, MonadError }
import com.jourei.compiler.data.*
import com.jourei.compiler.data.AST.*

import scala.annotation.tailrec

object SemanticAnalyzer {
  type ErrorOr[A] = Either[String, A]
  type ErrorOrWritable[A] = WriterT[ErrorOr, String, A]
  type WriterStringT[F[_], A] = WriterT[F, String, A]

  type MonadErrorString[F[_]] = MonadError[F, String]
  type ApplicativeErrorString[F[_]] = ApplicativeError[F, String]

  type ErrorableWriter[E, A] = WriterT[[a] =>> Either[E, a], String, A]

  /** 对AST进行语义分析
   *  @param program
   *    AST根
   *  @return
   */
  def analyzeProgram(program: Program): ErrorOrWritable[Unit] = {
    inline def globalScope =
      ScopedSymbolTable(
        "global",
        initBuiltinTypeSymbolTable(),
        List.empty,
        List.empty)

    for
      _ <- write(showProgramLine(program.name))
      _ <- analyzeBlock(program.block)(program.name)(
        initBuiltinTypeSymbolTable())(List.empty)(LStack(globalScope))
        .tell(". " + showEndOfScopeSegment(program.name))
    yield ()
  }

  private def buildScopedSymbolTable(name: String)(
      builtinTypeSymbolTable: BuiltinTypeSymbolTable)(
      varSymbolTable: VarSymbolTable)(
      procedureSymbolTable: ProcedureSymbolTable): ScopedSymbolTable =
    ScopedSymbolTable(
      name,
      builtinTypeSymbolTable,
      varSymbolTable,
      procedureSymbolTable)

  private type VarDecls = Seq[VarDecl]

  private def initBuiltinTypeSymbolTable(): BuiltinTypeSymbolTable =
    List(BuiltinTypeSymbol("INTEGER"), BuiltinTypeSymbol("REAL"))

  type Procedures = List[Procedure]
  type ScopeStack = LStack[ScopedSymbolTable]

  private def analyzeBlock(block: Block)(curScopeName: String)(
      builtinTypeSymbolTable: BuiltinTypeSymbolTable)(
      formalParamsOfCurLevel: VarDecls)(
      enclosingScopeStack: ScopeStack): ErrorOrWritable[Unit] =
    def guaranteeNonRepeatabilityAndFillCurrentLevelVarSymbolTable[
        F[_]: MonadErrorString](formalParamsOfCurLevel: VarDecls)(
        varDecls: VarDecls)(builtinTypeSymbolTable: BuiltinTypeSymbolTable)
        : WriterStringT[F, VarSymbolTable] =
      inline def showVarDecls(level: Int)(varDecls: VarDecls): String =
        varDecls.foldLeft("") { case (s, VarDecl(name, typeSpec)) =>
          s + s"VAR $name$level : ${typeSpec.name};\n"
        }

      val thisLevel = enclosingScopeStack.level.get + 1

      WriterT
        .liftF(
          guaranteeNonRepeatabilityAndBuildVarSymbolTable(
            formalParamsOfCurLevel ++ varDecls)(builtinTypeSymbolTable))
        .tell(showVarDecls(thisLevel)(varDecls).inLevel(thisLevel))
    end guaranteeNonRepeatabilityAndFillCurrentLevelVarSymbolTable

    def guaranteeNonRepeatabilityOfProcedures[F[_]: Applicative](
        procedures: Procedures)(using Raise[F, Unit]): F[Unit] =
      inline def notDuplicate(inline procedures: Procedures): Boolean =
        @tailrec
        def pairwiseCheck(procedures: Procedures): Boolean =
          procedures match
            case p1 :: (tail @ p2 :: _) =>
              (p1.name != p2.name) && pairwiseCheck(tail)
            case _ => true

        pairwiseCheck(procedures.sortBy(_.name))
      end notDuplicate

      if notDuplicate(procedures) then ().pure
      else summon[Raise[F, Unit]].raise(())
    end guaranteeNonRepeatabilityOfProcedures

    final case class VarDef(
        name: String,
        level: Int,
        vType: String,
        vTypeLevel: Int)
    def analyzeCompound(compound: Compound)(
        scopeStack: ScopeStack): ErrorOrWritable[Compound] =
      inline def containsVarOrError(varName: String)(
          scopeStack: ScopeStack): ErrorOrWritable[Unit] =
        WriterT(
          containsVarDef(varName)(scopeStack)
            .toRight(s"Variable $varName has not been declared")
            .map { case VarDef(name, level, vType, vTypeLevel) =>
              (showVariableWithType(name + level)(vType + vTypeLevel), ())
            })

      def findLeveledTypeInfo(
          typeName: String): ReaderT[Option, ScopeStack, Int] =
        def firstTime(typeName: String): ReaderT[Option, ScopeStack, Int] =
          Kleisli(scopeStack =>
            scopeStack.head.builtinTypeSymbolTable
              .find(typeName == _.name)
              .as(scopeStack.level.get))

        def remaining(typeName: String): ReaderT[Option, ScopeStack, Int] =
          Kleisli(scopeStack =>
            if scopeStack.isEmpty then Option.empty
            else
              scopeStack.head.builtinTypeSymbolTable.tailRecM[Option, Int] {
                builtinTypeSymbolTable =>
                  builtinTypeSymbolTable.find(typeName == _.name) match {
                    case Some(_) => Some(Right(scopeStack.level.get))
                    case None    => Some(Left(builtinTypeSymbolTable.tail))
                  }
              }
          )
        end remaining

        firstTime(typeName).combineK(remaining(typeName: String))
      end findLeveledTypeInfo

      def containsVarDef(varName: String)(
          scopeStack: ScopeStack): Option[VarDef] =
        if scopeStack.isEmpty then Option.empty
        else
          (for
            VarSymbol(name, vType) <- scopeStack.head.varSymbolTable.find(
              varName == _.name)
            vTypeLevel <- findLeveledTypeInfo(vType).run(scopeStack)
          yield VarDef(name, scopeStack.level.get, vType, vTypeLevel))
            .orElse(containsVarDef(varName)(scopeStack.pop()))
      end containsVarDef

      def checkVarDeclaredInStatement(scopeStack: ScopeStack)(
          statement: Statement): ErrorOrWritable[Unit] =
        statement match
          case Statement.Assign(variable, expr) =>
            def checkVarDeclaredInExpr(expr: Expr)(
                scopeStack: ScopeStack): ErrorOrWritable[Unit] =
              expr match
                case Expr.Variable(name) =>
                  containsVarOrError(name)(scopeStack)
                case Expr.Unary(op, child) =>
                  inline def writeUOp[F[_]: Applicative](
                      inline op: Expr.UOp): WriterT[F, String, Unit] =
                    write(op match
                      case Expr.UOp.Plus  => "+"
                      case Expr.UOp.Minus => "-"
                    )

                  for
                    _ <- writeUOp(op)
                    _ <- checkVarDeclaredInExpr(child)(scopeStack)
                  yield ()
                case Expr.Binary(op, lChild, rChild) =>
                  inline def showBOp(inline op: Expr.BOp): String =
                    op match
                      case Expr.BOp.Plus       => "+"
                      case Expr.BOp.Minus      => "-"
                      case Expr.BOp.FloatDiv   => "/"
                      case Expr.BOp.IntegerDiv => " div "
                      case Expr.BOp.Mul        => "*"

                  for
                    _ <- checkVarDeclaredInExpr(lChild)(scopeStack)
                      .tell(showBOp(op))
                    _ <- checkVarDeclaredInExpr(rChild)(scopeStack)
                  yield ()
                case Expr.Integer(v) => write(s"$v")
                case Expr.Real(v)    => write(s"$v")
              end match

            inline def trimMinus(inline s: String): String =
              s.replaceAll("""(?<=>.*)(?:--|\+)(?=.*<)""", "")
                .replaceAll("""(?<=>)(?=<)""", "+")

            for
              _ <- containsVarOrError(variable.name)(scopeStack)
                .mapWritten(_.inLevel(1 + scopeStack.level.get))
                .tell(" := ")
              _ <- checkVarDeclaredInExpr(expr)(scopeStack)
                .mapWritten(trimMinus(_))
                .tell(";\n")
            yield ()
          case Statement.InnerCompound(compound) =>
            for
              _ <- write("BEGIN\n".inLevel(scopeStack.level.get))
              (_, written) <- checkVarDeclaredInCompound(scopeStack)(compound)
                .map(_ => compound)
                .listen
              _ <- write(
                (if written.isEmpty then "\n" else "") + "END;\n".inLevel(
                  scopeStack.level.get))
            yield ()
          case Statement.NoOp => write("")
        end match

      def checkVarDeclaredInCompound(scopeStack: ScopeStack)(
          compound: Compound): ErrorOrWritable[Unit] =
        compound.statements.traverse_(checkVarDeclaredInStatement(scopeStack))

      for
        _ <- write("BEGIN\n".inLevel(scopeStack.level.get))
        (_, written) <- checkVarDeclaredInCompound(scopeStack)(compound)
          .map(_ => compound)
          .listen
        _ <- write[ErrorOr](
          ((if written.isEmpty then "\n" else "") + "END")
            .inLevel(scopeStack.level.get))
      yield compound
    end analyzeCompound

    def analyzeProcedures(procedures: Seq[Procedure])(
        scopeStack: ScopeStack): ErrorOrWritable[Unit] = {
      inline def toVarDecls(formalParams: Seq[FormalParam]) =
        formalParams.map { case FormalParam(name, typeSpec) =>
          VarDecl(name, typeSpec)
        }

      val thisLevel = scopeStack.level.get
      procedures.traverse_(procedure =>
        for {
          _ <- write[ErrorOr](
            showProcedureLine(thisLevel)(procedure.name)(procedure.formalParams)
              .inLevel(thisLevel))
          _ <- analyzeBlock(procedure.block)(procedure.name)(
            initBuiltinTypeSymbolTable())(toVarDecls(procedure.formalParams))(
            scopeStack).tell("; " + showEndOfScopeSegment(procedure.name))
        } yield ())
    }

    for
      varSymbolTableOfCurrentLevel <-
        guaranteeNonRepeatabilityAndFillCurrentLevelVarSymbolTable(
          formalParamsOfCurLevel)(block.varDecls)(builtinTypeSymbolTable)
      _ <- WriterT.liftF[ErrorOr, String, Unit](
        guaranteeNonRepeatabilityOfProcedures[Option](block.procedures.toList)
          .toRight("procedure name duplicate"))
      procedureSymbolTable = block.procedures.map(procedure =>
        ProcedureSymbol(procedure.name))
      currentScopeStack = enclosingScopeStack.push(
        buildScopedSymbolTable(curScopeName)(builtinTypeSymbolTable)(
          varSymbolTableOfCurrentLevel)(procedureSymbolTable))
      _ <- analyzeProcedures(block.procedures)(currentScopeStack)
      _ <- analyzeCompound(block.compound)(currentScopeStack)
    yield ()
  end analyzeBlock

  private type VarSymbolTable = Seq[VarSymbol]

  /** 检查是否有同名变量被声明，并生成变量符号表
   *  @param varDecls
   *    变量声明
   *  @param builtinTypeSymbolTable
   *    原始类型符号表
   *  @return
   *    变量符号表
   */
  private def guaranteeNonRepeatabilityAndBuildVarSymbolTable[
      F[_]: MonadErrorString](varDecls: VarDecls)(
      builtinTypeSymbolTable: BuiltinTypeSymbolTable): F[VarSymbolTable] = {
    def doOnce[F[_]: MonadErrorString](
        builtinTypeSymbolTable: BuiltinTypeSymbolTable)(variable: ID)(
        typeSpec: TypeSpec)(
        varSymbolTable: VarSymbolTable): F[VarSymbolTable] = {
      inline def notYetExists[F[_]: ApplicativeErrorString](variable: ID)(
          inline varSymbolTable: VarSymbolTable): F[ID] =
        if !varSymbolTable.contains(variable.name) then variable.pure
        else s"The variable ${variable.name} has been declared".raiseError

      def isValidTypeSpec[F[_]: ApplicativeErrorString](typeSpec: TypeSpec)(
          builtinTypeSymbolTable: BuiltinTypeSymbolTable): F[Unit] = {
        extension (typeSpec: TypeSpec)
          inline def toBuiltinTypeSymbol: BuiltinTypeSymbol =
            BuiltinTypeSymbol(typeSpec.name)

        if builtinTypeSymbolTable.contains(typeSpec.toBuiltinTypeSymbol) then
          ().pure
        else s"invalid type \"$typeSpec\"".raiseError
      }

      for {
        variable <- notYetExists(variable)(varSymbolTable)
        _ <- isValidTypeSpec(typeSpec)(builtinTypeSymbolTable)
      } yield varSymbolTable.prepended(VarSymbol(variable.name, typeSpec.name))
    }

    varDecls.foldM(Seq.empty) {
      case (varSymbolTable, VarDecl(variable, typeSpec)) =>
        doOnce(builtinTypeSymbolTable)(ID(variable))(typeSpec)(varSymbolTable)
    }
  }

  private inline def showFormalParams(inline level: Int)(
      formalParams: Seq[FormalParam]) =
    if formalParams.isEmpty then ""
    else
      "(" + formalParams
        .foldLeft("") { case (s, FormalParam(name, typeSpec)) =>
          s"$s, $name$level : ${typeSpec.name}"
        }
        .drop(1)
        .trim + ")"

  private inline def showProcedureLine(level: Int)(
      inline procedureName: String)(
      inline formalParams: Seq[FormalParam]): String =
    "PROCEDURE " + procedureName + level + showFormalParams(level + 1)(
      formalParams) + ";\n"

  private inline def showVariableWithType(inline name: String)(
      inline vType: String): String =
    s"<$name:$vType>"

  private inline def showProgramLine(inline programName: String): String =
    "PROGRAM " + programName + 0 /*lowestLevel*/ + ";\n"

  extension (s: String)
    inline def inLevel(inline level: Int): String =
      s.split('\n').map(showIndentation(0 max level) + _).mkString("\n")
      + (if s.endsWith("\n") then "\n" else "")
  end extension

  private inline def showIndentation(inline level: Int): String =
    List.fill(level)("  ").mkString

  private inline def showEndOfScopeSegment(inline scopeName: String): String =
    s"{END OF $scopeName}\n"

  private inline def write[F[_]: Applicative](
      inline s: String): WriterT[F, String, Unit] =
    WriterT((s, ()).pure)
}
