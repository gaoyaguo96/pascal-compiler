package com.jourei.compiler.parser

import cats.data.{ IndexedState, IndexedStateT, StateT }
import cats.mtl.syntax.handle.*
import cats.mtl.syntax.raise.*
import cats.mtl.syntax.state.*
import cats.mtl.{ Handle, Raise, Stateful }
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{ Applicative, FlatMap, Functor, Monad, MonadError }
import com.jourei.compiler.data.AST.*
import com.jourei.compiler.data.AST.Expr.Variable
import com.jourei.compiler.data.AST.Statement.InnerCompound
import com.jourei.compiler.data.error.ParserError
import com.jourei.compiler.data.{ LocatedToken, Token }

import scala.annotation.tailrec
import scala.util.chaining.*
import cats.data.OptionTMonad
import cats.Eval

object Parser:
  private type LocatedTokensStateT[F[_]] = [a] =>> StateT[F, LocatedTokens, a]

  final def parse[F[_]: Monad: HandleParserError](
      locatedTokens: LocatedTokens): F[Program] =
    implicit class PSHMC[F[_]: Monad, S, E](using Stateful[F, S])(
        using Handle[F, E])
        extends Monad[F]
        with Stateful[F, S]
        with Handle[F, E]:
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
      def pure[A](x: A): F[A] = Monad[F].pure(x)
      def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
        Monad[F].tailRecM(a)(f)
      def monad: Monad[F] = Monad[F]
      def get: F[S] = Stateful.get
      def set(s: S): F[Unit] = Stateful.set(s)
      def applicative: Applicative[F] = Monad[F]
      def handleWith[A](fa: F[A])(f: E => F[A]): F[A] =
        summon[Handle[F, E]].handleWith(fa)(f)
      def raise[E2 <: E, A](e: E2): F[A] = summon[Handle[F, E]].raise(e)
    end PSHMC

    program[[a] =>> StateT[F, LocatedTokens, a]].runA(locatedTokens)
  end parse

  type LocatedTokens = Seq[LocatedToken]

  private type StatefulLocatedTokens[F[_]] = Stateful[F, LocatedTokens]
  private type RaiseParserError[F[_]] = Raise[F, ParserError]
  private type HandleParserError[F[_]] = Handle[F, ParserError]

  private type ParserStatefulRaise[F[_]] = StatefulLocatedTokens[F] &
    RaiseParserError[F]
  private type ParserStatefulHandle[F[_]] = StatefulLocatedTokens[F] &
    HandleParserError[F]
  private type ParserStatefulRaiseMonad[F[_]] = Monad[F] &
    ParserStatefulRaise[F]
  private type ParserStatefulHandleMonad[F[_]] = Monad[F] &
    ParserStatefulHandle[F]
  private type ParserStatefulMonad[F[_]] = Monad[F] & StatefulLocatedTokens[F]

  infix type hasStateHandleMonadEff[A, F[_]] =
    (A hasEff F)[ParserStatefulHandleMonad]
  infix type hasStateMonadEff[A, F[_]] = (A hasEff F)[ParserStatefulMonad]
  infix type hasStateRaiseEff[A, F[_]] = (A hasEff F)[ParserStatefulRaise]
  infix type hasStateEff[A, F[_]] = (A hasEff F)[StatefulLocatedTokens]
  infix type hasStateRaiseMonadEff[A, F[_]] =
    (A hasEff F)[ParserStatefulRaiseMonad]

  infix type hasEff[A, F[_]] = [TC[_[_]]] =>> TC[F] ?=> F[A]

  type WithStateRaiseMonadEff[A] =
    [F[_]] =>> (A hasEff F)[ParserStatefulRaiseMonad]

  private inline def eat[F[_]: FlatMap: ParserStatefulRaise](
      inline token: Token): F[Unit] =
    for
      locatedTokens <- Stateful.get
      _ <-
        if locatedTokens.isEmpty then ParserError.UnexpectedEOF.raise
        else
          val head = locatedTokens.head
          if token == head.token then ParserStates.dropTokenS(1)
          else ParserError.UnexpectedToken(head).raise
    yield ()

  private def varTypeDecl[F[_]]: VarDecls hasStateHandleMonadEff F =
    type Vars = List[String]

    inline def collectRemainingVars: F[Vars] =
      List.empty[String].tailRecM[F, Vars] { vars =>
        eat(Token.Comma)
          .flatMap(_ => variable[F]) /* once */
          .attemptHandle
          .map {
            case Left(_)         => vars.asRight
            case Right(variable) => vars.prepended(variable.name).asLeft
          }
      }

    inline def assembleVarDecl(typeSpec: TypeSpec)(ids: Vars): VarDecls =
      ids.map(VarDecl(_, typeSpec))

    def toTypeSpec: PartialFunction[Token, TypeSpec] =
      case Token.Integer => TypeSpec("INTEGER")
      case Token.Real    => TypeSpec("REAL")

    for
      id <- variable
      ids <- collectRemainingVars
      _ <- eat(Token.Colon)
      typeSpec <- dealWithFirstTokenAndProcess(toTypeSpec)
      varDecls = assembleVarDecl(typeSpec)(id.name :: ids)
    yield varDecls
  end varTypeDecl

  private def block[F[_]]: Block hasStateHandleMonadEff F = {
    def varDeclarationBlock: F[VarDecls] =
      inline def varDeclarationLine: F[VarDecls] =
        varTypeDecl <* eat(Token.Semi)

      inline def collectOtherVarDecls: F[VarDecls] =
        List.empty[VarDecl].tailRecM[F, VarDecls] { varDecls =>
          varDeclarationLine.attemptHandle.map {
            case Left(_)            => varDecls.asRight
            case Right(newVarDecls) => (varDecls ++ newVarDecls).asLeft
          }
        }

      for
        locatedTokens <- Stateful.get
        varDecls <-
          if locatedTokens.nonEmpty && locatedTokens.head.token == Token.Var then
            for
              _ <- ParserStates.dropTokenS(1)
              varDecls <- varDeclarationLine
              otherVarDecls <- collectOtherVarDecls
            yield varDecls ++ otherVarDecls
          else List.empty.pure
      yield varDecls
    end varDeclarationBlock

    type Procedures = Seq[Procedure]

    def collectProcedures: F[Procedures] =
      List.empty[Procedure].tailRecM[F, Procedures] { reversedProcedures =>
        procedureDeclarationBlock.attemptHandle.map(
          _.fold(
            Function.const(reversedProcedures.reverse.asRight),
            procedure => (procedure +: reversedProcedures).asLeft))
      }

    for
      typeDefs <- varDeclarationBlock
      procedures <- collectProcedures
      compound <- compoundStatement
    yield Block(typeDefs, procedures, compound)
  }

  private def compoundStatement[F[_]]: Compound hasStateHandleMonadEff F = {
    def statementList: F[Statements] = {
      inline def empty[F[_]: Applicative]: Statement hasStateEff F =
        Statement.NoOp.pure

      def statement: F[Statement] = {
        for
          locatedTokens <- Stateful.get
          statement <-
            if locatedTokens.nonEmpty then
              locatedTokens.head.token match
                case Token.Begin =>
                  compoundStatement.map(InnerCompound.apply)
                case Token.ID(_) =>
                  def assignStatement: F[Statement] = {
                    def varExpr: F[Expr] =
                      inline def idToVarExpr: PartialFunction[Token, Expr] =
                        case Token.ID(v) => Expr.Variable(v)

                      dealWithFirstTokenAndProcess(idToVarExpr)

                    def factor: F[Expr] =
                      inline def factorWithUnary(
                          inline unary: Expr.UOp): F[Expr] =
                        factor.map(Expr.Unary(unary, _))

                      for
                        locatedTokens <- Stateful.get
                        expr <-
                          if locatedTokens.nonEmpty then
                            locatedTokens.head.token match
                              case Token.Plus =>
                                ParserStates.dropTokenS(1) *> factorWithUnary(
                                  Expr.UOp.Plus)
                              case Token.Minus =>
                                ParserStates.dropTokenS(1) *> factorWithUnary(
                                  Expr.UOp.Minus)
                              case Token.IntegerConst(v) =>
                                ParserStates
                                  .dropTokenS(1) *> Expr.Integer(v).pure
                              case Token.RealConst(v) =>
                                ParserStates.dropTokenS(1) *> Expr.Real(v).pure
                              case Token.ID(_)  => varExpr
                              case Token.LParen => expr <* eat(Token.RParen)
                              case _ =>
                                ParserError
                                  .UnexpectedToken(locatedTokens.head)
                                  .raise
                            end match
                          else ParserError.UnexpectedEOF.raise
                      yield expr
                    end factor

                    def term: F[Expr] =
                      def doCyclically: F[Expr => Expr] = 
                        type IncompleteExpr = Expr => Expr
                        identity[Expr].tailRecM[F, Expr => Expr] {
                          previousIncompleteExpr =>
                            inline def evalRight(
                                inline op: Expr.BOp): F[IncompleteExpr] =
                              for expr <- factor
                              yield Expr.Binary(op, _, expr)

                            def once: F[IncompleteExpr] =
                              for
                                locatedTokens <- Stateful.get
                                incompleteExpr <-
                                  if locatedTokens.isEmpty then
                                    ParserError.UnexpectedEOF.raise
                                  else
                                    extension (inline token: Token)
                                      inline def toBOp(
                                          inline e: ParserError): F[Expr.BOp] =
                                        token match
                                          case Token.Mul => Expr.BOp.Mul.pure
                                          case Token.IntegerDiv =>
                                            Expr.BOp.IntegerDiv.pure
                                          case Token.FloatDiv =>
                                            Expr.BOp.FloatDiv.pure
                                          case _ => e.raise
                                    end extension

                                    lazy val head = locatedTokens.head
                                    ParserStates.dropTokenS(1) *> head.token
                                      .toBOp(ParserError.UnexpectedToken(head))
                                      .flatMap(evalRight(_))
                              yield incompleteExpr

                            once.attemptHandle.map {
                              _.fold(
                                _ => previousIncompleteExpr.asRight,
                                previousIncompleteExpr.andThen(_).asLeft)
                            }
                        }
                      end doCyclically

                      for
                        expr <- factor
                        connectToLeft <- doCyclically
                      yield connectToLeft(expr)
                    end term

                    def expr: F[Expr] =
                      type BuildExprWithLeft = Expr => Expr

                      def doPlusOrMinusCyclically: F[BuildExprWithLeft] =
                        def once: F[BuildExprWithLeft] = {
                          def getBOp: F[Expr.BOp] =
                            for
                              locatedTokens <- Stateful.get
                              bOp <-
                                if locatedTokens.isEmpty then
                                  ParserError.UnexpectedEOF.raise
                                else
                                  locatedTokens.head.token match
                                    case Token.Plus  => Expr.BOp.Plus.pure
                                    case Token.Minus => Expr.BOp.Minus.pure
                                    case _ =>
                                      ParserError
                                        .UnexpectedToken(locatedTokens.head)
                                        .raise
                            yield bOp

                          def toBinary(bOp: Expr.BOp)(right: Expr)(
                              left: Expr): Expr =
                            Expr.Binary(bOp, left, right)

                          for
                            bOp <- getBOp
                            rExpr <- term
                          yield toBinary(bOp)(rExpr)
                        }

                        identity[Expr].tailRecM[F, BuildExprWithLeft] {
                          buildExprWithLeft =>
                            once.attemptHandle.map(
                              _.fold(
                                Function.const(buildExprWithLeft.asRight),
                                newExprLackingLeft =>
                                  buildExprWithLeft
                                    .andThen(newExprLackingLeft)
                                    .asLeft))
                        }
                      end doPlusOrMinusCyclically

                      for
                        lExpr <- term
                        buildExprWithLeft <- doPlusOrMinusCyclically
                      yield buildExprWithLeft(lExpr)
                    end expr

                    for
                      variable <- variable
                      _ <- eat(Token.Assign)
                      expr <- expr
                    yield Statement.Assign(variable, expr)
                  }

                  assignStatement
                case _ => empty
              end match
            else empty
        yield statement
      }

      for
        statement <- statement
        statements <- eat(Token.Semi).attemptHandle
          .flatMap(_.fold(Function.const(List.empty.pure), _ => statementList))
      yield statement :: statements
    }

    type Statements = List[Statement]

    for
      _ <- eat(Token.Begin)
      statements <- statementList
      _ <- eat(Token.End)
    yield Compound(statements)
  }

  private inline def checkEOF[F[_]]: Unit hasStateRaiseMonadEff F =
    for
      locatedTokens <- Stateful.get
      _ <-
        if locatedTokens.isEmpty then ParserError.UnexpectedEOF.raise
        else if Token.EOF == locatedTokens.head.token then Applicative[F].unit
        else ParserError.UnexpectedToken(locatedTokens.head).raise
    yield ()

  private inline def program[F[_]]: Program hasStateHandleMonadEff F =
    for
      _ <- eat(Token.Program)
      id <- variable
      _ <- eat(Token.Semi)
      block <- block
      _ <- eat(Token.Dot)
      _ <- checkEOF
    yield Program(id.name, block)
  end program

  private inline def procedureDeclarationBlock[F[_]]
      : Procedure hasStateHandleMonadEff F =
    for
      _ <- eat(Token.Procedure)
      variable <- variable
      params <- enclosedFormalParameterList[F]
        .handle[ParserError](Function.const(List.empty))
      _ <- eat(Token.Semi)
      block <- block
      _ <- eat(Token.Semi)
    yield Procedure(variable.name, params, block)

  private inline def enclosedFormalParameterList[F[_]]
      : Params hasStateHandleMonadEff F =
    for
      _ <- eat(Token.LParen)
      params <- formalParameterList
      _ <- eat(Token.RParen)
    yield params

  type Params = List[FormalParam]

  private inline def formalParameterList[F[_]]
      : Params hasStateHandleMonadEff F =
    List.empty[FormalParam].tailRecM[F, Params] { params =>
      formalParameter.attemptHandle[ParserError].map[Either[Params, Params]] {
        _.fold[Either[Params, Params]](
          Function.const(params.asRight[Params]),
          (varDecls: VarDecls) =>
            (varDecls.map { case VarDecl(name, typeSpec) =>
              FormalParam(name, typeSpec)
            } ++ params).toList.asLeft)
      }
    }
  end formalParameterList

  private inline def formalParameter[F[_]: ParserStatefulHandleMonad] =
    varTypeDecl[F]

  private def variable[F[_]: ParserStatefulRaiseMonad]
      : ID hasStateRaiseMonadEff F =
    inline def toID: PartialFunction[Token, ID] =
      case Token.ID(v) => ID(v)

    dealWithFirstTokenAndProcess(toID)

  private type VarDecls = Seq[VarDecl]

  inline def dealWithFirstTokenAndProcess[F[_], Out](
      inline dealer: PartialFunction[Token, Out]): Out hasStateRaiseMonadEff F =
    for
      locatedTokens <- Stateful.get
      selectedToken <-
        if locatedTokens.isEmpty then ParserError.UnexpectedEOF.raise
        else
          val first = locatedTokens.head
          if dealer.isDefinedAt(first.token) then
            dealer(first.token).pure <* locatedTokens.tail.set
          else ParserError.UnexpectedToken(first).raise
    yield selectedToken
  end dealWithFirstTokenAndProcess
end Parser
