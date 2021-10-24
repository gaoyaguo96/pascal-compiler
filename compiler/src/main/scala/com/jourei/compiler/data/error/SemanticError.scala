package com.jourei.compiler.data.error

import cats.Show

final case class SemanticError(
    semanticErrorCause: SemanticErrorCause,
    accidentScene: PositionInSource)

enum SemanticErrorCause:
  case DuplicateID(name: String)
  case IDNotFound(name: String)
