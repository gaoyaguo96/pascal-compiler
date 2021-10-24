package com.jourei.compiler.data

import com.jourei.compiler.data.error.PositionInSource

final case class LocatedToken(token: Token, positionInSource: PositionInSource)
