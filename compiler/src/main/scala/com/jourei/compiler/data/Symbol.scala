package com.jourei.compiler.data

final case class BuiltinTypeSymbol(builtinType: BuiltinType) extends AnyVal
final case class VarSymbol(name: String, vType: VType)

enum VType:
  case Builtin(value: BuiltinType)

enum BuiltinType:
  case Integer, Real
