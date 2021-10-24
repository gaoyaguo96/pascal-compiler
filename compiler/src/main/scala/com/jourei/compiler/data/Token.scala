package com.jourei.compiler.data

import cats.Show

enum Token:
  case Program, Var, Procedure, Colon, Comma, Integer, Real, Plus, Minus, Mul,
  IntegerDiv, FloatDiv, LParen, RParen, EOF, Begin, End, Dot, Assign, Semi
  case IntegerConst(v: Int)
  case RealConst(v: Float)
  case ID(v: String)

object Token:
  extension (inline s: String)
    inline def toKeywordTokenOrIDToken: Token =
      s match
        case "BEGIN"     => Token.Begin
        case "END"       => Token.End
        case "DIV"       => Token.IntegerDiv
        case "PROGRAM"   => Token.Program
        case "PROCEDURE" => Token.Procedure
        case "VAR"       => Token.Var
        case "INTEGER"   => Token.Integer
        case "REAL"      => Token.Real
        case s           => s.toIDToken
      end match
    end toKeywordTokenOrIDToken
    
    inline def toIDToken: Token = Token.ID(s)
  end extension
end Token
