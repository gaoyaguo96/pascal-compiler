package com.jourei.compiler.data

import com.jourei.compiler.data.BuiltinTypeSymbol

opaque type BuiltinTypeSymbolTable = Set[BuiltinTypeSymbol]

object BuiltinTypeSymbolTable:
  extension (st: BuiltinTypeSymbolTable)
    inline def define(symbol: BuiltinTypeSymbol): BuiltinTypeSymbolTable =
      st + symbol

opaque type VarSymbolTable = Map[String, VType]

object VarSymbolTable:
  def fromMap(map: Map[String, VType]): VarSymbolTable = map
  def fromTupleSeq(tuples: Seq[(String, VType)]): VarSymbolTable =
    tuples.toMap

  extension (st: VarSymbolTable)
    inline def add(name: String)(vType: VType): VarSymbolTable =
      st.updated(name, vType)

    def get(name: String): Option[VarSymbol] =
      inline def buildVarSymbol(name: String)(vType: VType) =
        VarSymbol(name, vType)

      st.get(name).map(buildVarSymbol(name)(_))
