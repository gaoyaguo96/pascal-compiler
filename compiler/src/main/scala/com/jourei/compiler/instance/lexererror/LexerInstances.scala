package com.jourei.compiler.instance.lexererror

import cats.Show
import com.jourei.compiler.data.error.LexerError
import com.jourei.compiler.typeclass.AboutError

given AboutError[LexerError] with
  type Detail = String
  extension (e: LexerError)
    inline def lineNo: Int = e.accidentScene.lineNo
    inline def column: Int = e.accidentScene.column
    inline def errorType: Detail = "Lexer error"
    inline def detail: Detail = e.lexeme
