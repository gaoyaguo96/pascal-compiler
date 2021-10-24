package com.jourei.compiler.typeclass

trait CanBeLocatedToSource[-A]:
  extension (a: A)
    def lineNo: Int
    def column: Int
