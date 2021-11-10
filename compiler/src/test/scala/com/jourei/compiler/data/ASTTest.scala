package com.jourei.compiler.data

import cats.syntax.either.*
import cats.syntax.option.*
import cats.data.{EitherT, State}
import com.jourei.compiler.lexer.LexerStates.Position
import com.jourei.compiler.data.error.{LexerError, ParserError}
import com.jourei.compiler.parser.Parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.data.StateT
import com.jourei.compiler.lexer.Lexer

class ASTTest extends AnyFlatSpec with Matchers:
  "AST" should "pass the basic test" in {
    import scala.util.chaining.*
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
    val tokens = Lexer
      .doLexicalAnalysis[
        [b] =>> EitherT[[a] =>> State[Position, a], LexerError, b]](str)
      .tap(_.value.run(Position(1, 1)).value.pipe(println))

    val ast = Parser
      .parse[[a] =>> Either[ParserError, a]](
        tokens.getOrElse(List()).runA(Position(1, 1)).value)
      .tap(println)

    println(ast)
    //    val ast1 = SemanticAnalyzer.analyzeProgram(ast.toOption.get).tap(println)
    //    val symbolTable =
    //      Interpreter.interpret(ast.toOption.get).tap(println)
  }
end ASTTest