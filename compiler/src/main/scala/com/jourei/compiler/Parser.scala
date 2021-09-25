package com.jourei.compiler

import cats.data.{ State, StateT }
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.option.*
import com.jourei.compiler.data.AST.*
import com.jourei.compiler.data.AST.Expr.Variable
import com.jourei.compiler.data.AST.Statement.InnerCompound

import scala.annotation.tailrec
import scala.util.chaining.*

object Parser {
  final def parse(tokens: Tokens): ErrorOr[Program] = program(tokens)

  type AndTokenAcc[a] = (a, Tokens)
  type OptionalTokenAcc[a] = Option[AndTokenAcc[a]]
  type ErrorableTokenAcc[a] = ErrorOr[AndTokenAcc[a]]

  private inline def eatProcedure(tokens: Tokens): Option[Tokens] =
    eat(Token.Procedure)(tokens)

  private def eat(token: Token)(tokens: Tokens): Option[Tokens] =
    tokens match
      case head :: tokens if head == token => Some(tokens)
      case _                               => Option.empty

  private inline def eatSemi(tokens: Tokens): Option[Tokens] =
    eat(Token.Semi)(tokens)

  private def block(tokens: Tokens): ErrorableTokenAcc[Block] = {
    def varDeclarationBlock(tokens: Tokens): ErrorableTokenAcc[TypeDefs] = {
      tokens match
        case Token.Var :: tokens =>
          def varTypeDecl(tokens: Tokens): ErrorableTokenAcc[TypeDefs] = {
            def typeSpec(tokens: Tokens): OptionalTokenAcc[TypeSpec] =
              tokens match
                case Token.Integer :: tokens =>
                  (TypeSpec.Integer, tokens).some
                case Token.Real :: tokens => (TypeSpec.Real, tokens).some
                case _                    => Option.empty

            type Vars = List[Var]

            def getID(tokens: Tokens): OptionalTokenAcc[Var] =
              tokens match
                case Token.ID(v) :: tokens => (Var(v), tokens).some
                case _                     => Option.empty

            def collectRemainingVars(tokens: Tokens): AndTokenAcc[Vars] = {
              def loop(tokens: Tokens)(acc: Vars): AndTokenAcc[Vars] = {
                inline def once(tokens: Tokens): OptionalTokenAcc[Var] =
                  def eatComma(tokens: Tokens): Option[Tokens] =
                    eat(Token.Comma)(tokens)

                  eatComma(tokens).flatMap(getID)

                once(tokens) match
                  case None => (acc, tokens)
                  case Some((variable, tokens)) =>
                    loop(tokens)(variable :: acc)
              }

              loop(tokens)(List.empty)
            }

            inline def eatColon(tokens: Tokens): Option[Tokens] =
              eat(Token.Colon)(tokens)

            inline def getTypeSpec(tokens: Tokens): OptionalTokenAcc[TypeSpec] =
              tokens match
                case Token.Integer :: tokens =>
                  (TypeSpec.Integer, tokens).some
                case Token.Real :: tokens => (TypeSpec.Real, tokens).some
                case _                    => Option.empty

            inline def assembleTypeDef(typeSpec: TypeSpec)(
                ids: Vars): TypeDefs =
              TypeDefs(ids.map((_, typeSpec)).toMap)

            inline def eatVar(tokens: Tokens): Option[Tokens] =
              eat(Token.Var)(tokens)

            for {
              (id, tokens) <- getID(tokens).toRight("ID missing")
              (ids, tokens) <- collectRemainingVars(tokens).asRight
              tokens <- eatColon(tokens).toRight("COLON missing")
              (typeSpec, tokens) <- getTypeSpec(tokens).toRight(
                "TYPESPEC missing")
              typeDefs = assembleTypeDef(typeSpec)(id :: ids)
            } yield (typeDefs, tokens)
          }

          def varDeclarationLines(
              tokens: Tokens): ErrorableTokenAcc[TypeDefs] = {
            def varDeclarationLine(
                tokens: Tokens): ErrorableTokenAcc[TypeDefs] =
              for
                (typeDefs, tokens) <- varTypeDecl(tokens)
                tokens <- eatSemi(tokens).toRight("SEMI missing")
              yield (typeDefs, tokens)

            inline def collectOtherTypeDefs(
                tokens: Tokens): AndTokenAcc[TypeDefs] = {
              def loop(tokens: Tokens)(
                  previousTypeDefs: TypeDefs): AndTokenAcc[TypeDefs] =
                varDeclarationLine(tokens) match
                  case Left(_) => (previousTypeDefs, tokens)
                  case Right((typeDefs, tokens)) =>
                    loop(tokens)(previousTypeDefs ++ typeDefs)

              loop(tokens)(TypeDefs.empty)
            }

            for {
              (typeDefs, tokens) <- varDeclarationLine(tokens)
              (otherTypeDefs, tokens) <- collectOtherTypeDefs(tokens).asRight
            } yield (typeDefs ++ otherTypeDefs, tokens)
          }

          varDeclarationLines(tokens)
        case _ => (TypeDefs.empty, tokens).asRight
      end match
    }

    def collectProcedures(tokens: Tokens): AndTokenAcc[Procedures] = {

      inline def once(tokens: Tokens): OptionalTokenAcc[Procedure] =
        procedureDeclarationBlock(tokens).toOption

      def loop(procedures: List[Procedure])(
          tokens: Tokens): AndTokenAcc[Procedures] =
        once(tokens) match
          case None => (Procedures(procedures.reverse), tokens)
          case Some((procedure, tokens)) =>
            loop(procedure :: procedures)(tokens)

      loop(List.empty)(tokens)
    }

    for {
      (typeDefs, tokens) <- varDeclarationBlock(tokens)
      (procedures, tokens) <- collectProcedures(tokens).asRight
      (compound, tokens) <- compoundStatement(tokens)
    } yield (Block(typeDefs, procedures, compound), tokens)
  }

  private def compoundStatement(
      tokens: Tokens): ErrorOr[AndTokenAcc[Compound]] = {
    inline def eatBegin(tokens: Tokens): Option[Tokens] =
      eat(Token.Begin)(tokens)

    def statementList(tokens: Tokens): ErrorOr[AndTokenAcc[Statements]] = {
      inline def empty(tokens: Tokens): AndTokenAcc[Statement] =
        (Statement.NoOp, tokens)

      def statement(tokens: Tokens): ErrorOr[AndTokenAcc[Statement]] =  {
        tokens match
          case Token.Begin :: _ =>
            compoundStatement(tokens).map(_.leftMap(InnerCompound.apply))
          case Token.ID(_) :: _ =>
            inline def eatAssign(tokens: Tokens): Option[Tokens] =
              eat(Token.Assign)(tokens)

            def assignStatement(
                tokens: Tokens): ErrorOr[AndTokenAcc[Statement]] = {
              inline def varExpr(tokens: Tokens): Option[AndTokenAcc[Expr]] =
                tokens match
                  case Token.ID(v) :: tokens =>
                    (Variable(v), tokens).some
                  case _ => Option.empty

              def factor(tokens: Tokens): ErrorOr[AndTokenAcc[Expr]] = {
                inline def factorWithUnary(unary: Expr.UOp)(
                    tokens: Tokens): ErrorOr[AndTokenAcc[Expr]] =
                  factor(tokens).map { case (expr, tokens) =>
                    (Expr.Unary(unary, expr), tokens)
                  }

                tokens match {
                  case Token.Plus :: tokens =>
                    factorWithUnary(Expr.UOp.Plus)(tokens)
                  case Token.Minus :: tokens =>
                    factorWithUnary(Expr.UOp.Minus)(tokens)
                  case Token.IntegerConst(v) :: tokens =>
                    (Expr.Integer(v), tokens).asRight
                  case Token.RealConst(v) :: tokens =>
                    (Expr.Real(v), tokens).asRight
                  case Token.ID(_) :: _ =>
                    varExpr(tokens).toRight("ID missing")
                  case Token.LParen :: tokens =>
                    for {
                      (expr, tokens) <- expr(tokens)
                      tokens <- eat(Token.RParen)(tokens)
                        .toRight("RParen missing")
                    } yield (expr, tokens)
                  case _ => "Can not find a factor".asLeft
                }
              }

              def term(tokens: Tokens): ErrorOr[AndTokenAcc[Expr]] = {
                enum CyclicalError:
                  case NoCycle
                  case Otherwise(cause: String)

                def doCyclically(tokens: Tokens): AndTokenAcc[Expr => Expr] = {
                  type IncompleteExpr = Expr => Expr
                  @tailrec
                  def loop(tokens: Tokens)(
                      previousIncompleteExpr: IncompleteExpr)
                      : AndTokenAcc[IncompleteExpr] = {
                    def once(
                        tokens: Tokens): OptionalTokenAcc[IncompleteExpr] = {
                      tokens match
                        case token :: tokens =>
                          extension (token: Token)
                            inline def toBOp: Option[Expr.BOp] =
                              token match
                                case Token.Mul => Expr.BOp.Mul.some
                                case Token.IntegerDiv =>
                                  Expr.BOp.IntegerDiv.some
                                case Token.FloatDiv =>
                                  Expr.BOp.FloatDiv.some
                                case _ => Option.empty

                          for
                            bOp <- token.toBOp
                            acc <- evalRight(bOp)(tokens)
                          yield acc
                        case _ => Option.empty
                      end match
                    }

                    inline def evalRight(op: Expr.BOp)(
                        tokens: Tokens): OptionalTokenAcc[IncompleteExpr] =
                      for (expr, tokens) <- factor(tokens).toOption
                      yield (Expr.Binary(op, _, expr), tokens)

                    once(tokens) match
                      case None => (previousIncompleteExpr, tokens)
                      case Some((incompleteExpr, tokens)) =>
                        loop(tokens)(
                          previousIncompleteExpr andThen incompleteExpr)
                  }

                  loop(tokens)(identity)
                }

                for {
                  (expr, tokens) <- factor(tokens)
                  (connectToLeft, tokens) <- doCyclically(tokens).asRight
                } yield (connectToLeft(expr), tokens)
              }

              def expr(tokens: Tokens): ErrorOr[AndTokenAcc[Expr]] = {
                type BuildExprWithRight = Expr => Expr

                inline def plusOrMinus(tokens: Tokens)(
                    expr: Expr): Option[AndTokenAcc[BuildExprWithRight]] =
                  tokens match {
                    case Token.Plus :: tokens =>
                      (Expr.Binary(Expr.BOp.Plus, expr, _), tokens).some
                    case Token.Minus :: tokens =>
                      (Expr.Binary(Expr.BOp.Minus, expr, _), tokens).some
                    case _ => Option.empty
                  }

                for
                  (expression, tokens) <- term(tokens)
                  (expr, tokens) <- plusOrMinus(tokens)(expression)
                    .fold((expression, tokens).asRight[String]) {
                      case (buildExprWithRight, tokens) =>
                        expr(tokens).map { case (expr, tokens) =>
                          (buildExprWithRight(expr), tokens)
                        }
                    }
                yield (expr, tokens)
              }

              for
                (variable, tokens) <- variable(tokens).toRight(
                  "Variable missing")
                tokens <- eatAssign(tokens).toRight("ASSIGN missing")
                (expr, tokens) <- expr(tokens)
              yield (Statement.Assign(variable, expr), tokens)
            }

            assignStatement(tokens)
          case _ => empty(tokens).asRight
        end match
      }

      for {
        (statement, tokens) <- statement(tokens)
        (statements, tokens) <- eatSemi(tokens).fold(
          (List.empty, tokens).asRight)(statementList)
      } yield (statement :: statements, tokens)
    }

    inline def eatEnd(tokens: Tokens): Option[Tokens] = eat(Token.End)(tokens)

    type Statements = List[Statement]

    for {
      tokens <- eatBegin(tokens).toRight("BEGIN missing")
      (statements, tokens) <- statementList(tokens)
      tokens <- eatEnd(tokens).toRight("END missing")
    } yield (Compound(statements), tokens)
  }

  private inline def eatDot(tokens: Tokens): Option[Tokens] =
    eat(Token.Dot)(tokens)

  private inline def checkEOF(tokens: Tokens): Option[Tokens] =
    tokens match
      case Token.EOF :: Nil => Some(tokens)
      case _                => Option.empty

  private inline def eatProgram(tokens: Tokens): Option[Tokens] =
    eat(Token.Program)(tokens)

  private def program(tokens: Tokens): ErrorOr[Program] = {
    type TypeSpecTable = Map[String, TypeSpec]

    for {
      tokens <- eatProgram(tokens).toRight("PROGRAM missing")
      (name, tokens) <- variable(tokens).toRight("ID missing")
      tokens <- eatSemi(tokens).toRight("SEMI missing")
      (block, tokens) <- block(tokens)
      tokens <- eatDot(tokens).toRight("DOT missing")
      _ <- checkEOF(tokens).toRight("EOF missing")
    } yield Program(name.toString, block)
  }

  private def procedureDeclarationBlock(
      tokens: Tokens): ErrorableTokenAcc[Procedure] =
    for {
      tokens <- eatProcedure(tokens).toRight("PROCEDURE missing")
      (name, tokens) <- variable(tokens).toRight("ID missing")
      tokens <- eatSemi(tokens).toRight("SEMI missing")
      (block, tokens) <- block(tokens)
      tokens <- eatSemi(tokens).toRight("SEMI missing")
    } yield (Procedure(name.toStr, block), tokens)

  private def variable(tokens: Tokens): OptionalTokenAcc[Var] =
    tokens match
      case Token.ID(s) :: tokens => (Var(s), tokens).some
      case _                     => Option.empty

  private type Tokens = List[Token]
  type ErrorOr[a] = Either[String, a]
}
