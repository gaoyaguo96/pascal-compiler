package com.jourei.compiler.typeclass

import cats.Show
import com.jourei.compiler.data.error.PositionInSource

trait AboutError[A]:
  type Detail
  extension (a: A)
    def lineNo: Int
    def column: Int
    def errorType: Detail
    def detail: Detail
