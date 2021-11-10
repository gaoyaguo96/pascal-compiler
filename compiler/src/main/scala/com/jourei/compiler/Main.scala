package com.jourei.compiler

import cats.data.{ EitherT, State }
import com.jourei.compiler.data.LocatedToken
import com.jourei.compiler.data.error.{ LexerError, ParserError }
import com.jourei.compiler.lexer.Lexer
import com.jourei.compiler.lexer.LexerStates.Position
import com.jourei.compiler.parser.Parser
import com.jourei.compiler.semantic.SemanticAnalyzer

import java.io.File
import scala.io.{ Source, StdIn }
import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    println("Enter the absolute path of a source code file: ")
    var filePath: String = Console.in.readLine()
    while (!new File(filePath).exists()) {
      println("The file does not exist. Please try again.")
      filePath = Console.in.readLine()
    }

    val source = Source.fromFile(filePath).mkString
    val errorOrLeveledSource =
      for {
        tokens <- Lexer
          .doLexicalAnalysis[
            [b] =>> EitherT[[a] =>> State[Position, a], LexerError, b]](source)
          .value
          .runA(Position(1, 1))
          .value
          .left
          .map { case LexerError(lexeme, accidentScene) =>
            s"lexical error: line ${accidentScene.lineNo}, " +
              s"column ${accidentScene.column}, lexeme $lexeme"
          }
        ast <- Parser.parse[[a] =>> Either[ParserError, a]](tokens).left.map {
          case ParserError.UnexpectedToken(
                LocatedToken(token, positionInSource)) =>
            s"Invalid syntax: line ${positionInSource.lineNo}," +
              s" column ${positionInSource.column}"
          case ParserError.UnexpectedEOF =>
            s"The source file text is incomplete"
        }
        leveledSource <- SemanticAnalyzer.analyzeProgram(ast).written
      } yield leveledSource

    println(errorOrLeveledSource.fold(identity, _.toString))
  }
}
