package com.jourei.compiler.instance.accidentscene

import com.jourei.compiler.data.error.PositionInSource
import com.jourei.compiler.typeclass.CanBeLocatedToSource

given CanBeLocatedToSource[PositionInSource] with
  extension (accidentScene: PositionInSource)
    inline def lineNo: Int = accidentScene.lineNo
    inline def column: Int = accidentScene.column
