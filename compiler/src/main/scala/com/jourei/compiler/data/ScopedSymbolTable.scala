package com.jourei.compiler.data

final case class ScopedSymbolTable(
    name: String,
    builtinTypeSymbolTable: BuiltinTypeSymbolTable,
    varSymbolTable: VarSymbolTable,
    procedureSymbolTable: ProcedureSymbolTable)

final case class ProcedureSymbol(name: String) extends AnyVal
final case class VarSymbol(name: String, vType: String)

final case class BuiltinTypeSymbol(name: String) extends AnyVal

type BuiltinTypeSymbolTable = Seq[BuiltinTypeSymbol]

type VarSymbolTable = Seq[VarSymbol]

type ProcedureSymbolTable = Seq[ProcedureSymbol]
