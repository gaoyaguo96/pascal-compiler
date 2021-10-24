package com.jourei.compiler.data.error

import cats.Show
import com.jourei.compiler.data.Token
import com.jourei.compiler.data.LocatedToken

enum ParserError :
  case UnexpectedToken(locatedToken: LocatedToken)
  case UnexpectedEOF
