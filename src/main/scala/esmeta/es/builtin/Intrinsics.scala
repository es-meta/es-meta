package esmeta.es.builtin

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.spec.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*

/** model for intrinsics */
case class Intrinsics(cfg: CFG) {

  /** shortcuts */
  private def spec = cfg.program.spec
  given CFG = cfg

  // XXX remove after update to ES2025
  lazy val isES2024 = cfg.spec.version.exists(
    _.hash == "0b24a049c11fe0604b1c929772e7cbd671b78492",
  )
  def getGeneratorPrototypeName(async: String, postfix: String): String =
    if (isES2024) s"${async}GeneratorFunction.prototype.prototype$postfix"
    else s"${async}GeneratorPrototype$postfix"

  /** get map for heap */
  lazy val map: Map[Addr, Obj] = {
    var _map = Map[Addr, Obj]()

    // add builtin objects
    builtins.foreach {
      case (name, Struct(typeName, imap, nmap))
          if !(yets contains name.split("\\.").head) =>
        // base object
        _map += intrAddr(name) -> recordObj(typeName)(
          List(
            INNER_MAP -> mapAddr(intrName(name)),
            PRIVATE_ELEMENTS -> elemsAddr(intrName(name)),
          ) ++ imap: _*,
        )

        // map object
        _map ++= getMapObjects(intrName(name), name, nmap)
      case _ =>
    }

    // add symbols
    symbols.foreach { symField =>
      val addr = symbolAddr(symField)
      val obj = recordObj("Symbol")("Description" -> Str(symbolName(symField)))
      _map += addr -> obj
    }

    // not yet objects
    yets.foreach { (name, _) => _map += (intrAddr(name) -> YetObj(name, name)) }

    // result
    _map
  }

  lazy val kinds: Map[String, ValueTy] =
    val xs = (for {
      x <- names.toList
      addr = intrAddr(x)
      case (obj: RecordObj) <- map.get(addr)
      ty =
        if (obj.tname == "Symbol") SymbolT
        else if (obj.map contains "Construct") ConstructorT
        else if (obj.map contains "Call") FunctionT
        else ObjectT
    } yield x -> ty).toMap ++ yets
    xs.map { case (x, ty) => s"%$x%" -> ty }

  lazy val names: Set[String] =
    builtins.keySet ++
    symbols.map("Symbol." + _) ++
    yets.keySet
  private val pattern = "%Symbol.([^%]+)%".r
  lazy val symbols: List[String] = (for {
    row <- spec.tables(WELL_KNOWN_SYMBOLS).rows
    symbolField <- row.headOption.collect { case pattern(s) => s }
  } yield symbolField)

  /** get intrinsic map */
  val obj: MapObj = MapObj(names.toList.map(x => Str(s"%$x%") -> intrAddr(x)))

  // get closures
  private def clo(name: String): Clo = Clo(cfg.fnameMap(name), Map())
  private def intrClo(name: String): Clo = clo(intrName(name))

  // intrinsics
  // https://tc39.es/ecma262/#sec-ecmascript-standard-built-in-objects
  private lazy val builtins: Map[String, Struct] = errors ++ Map(
    "print" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "ScriptOrModule" -> Null,
        "Realm" -> realmAddr,
        INNER_CODE -> intrClo("print"),
        "Prototype" -> intrAddr("Function.prototype"),
        "InitialName" -> Str("print"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, T),
      ),
    ),
    "Object" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Object"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Object.prototype"), F, F, F),
      ),
    ),
    "Object.prototype" -> Struct(
      typeName = "ImmutablePrototypeExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Object"), T, F, T),
      ),
    ),
    "Object.assign" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Function" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("Function"), F, F, T),
        "prototype" -> DataProperty(intrAddr("Function.prototype"), F, F, F),
      ),
    ),
    "Function.prototype" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
        INNER_CODE -> intrClo("Function.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, T),
        "name" -> DataProperty(Str(""), F, F, T),
        "constructor" -> DataProperty(intrAddr("Function"), T, F, T),
        "@@hasInstance" -> DataProperty(
          intrAddr("Function.prototype[@@hasInstance]"),
          F,
          F,
          F,
        ),
      ),
    ),
    "Boolean" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Boolean.prototype"), F, F, F),
      ),
    ),
    "Boolean.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "BooleanData" -> Bool(false),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Boolean"), T, F, T),
      ),
    ),
    "Symbol" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "asyncIterator" -> DataProperty(symbolAddr("asyncIterator"), F, F, F),
        "hasInstance" -> DataProperty(symbolAddr("hasInstance"), F, F, F),
        "isConcatSpreadable" -> DataProperty(
          symbolAddr("isConcatSpreadable"),
          F,
          F,
          F,
        ),
        "iterator" -> DataProperty(symbolAddr("iterator"), F, F, F),
        "match" -> DataProperty(symbolAddr("match"), F, F, F),
        "matchAll" -> DataProperty(symbolAddr("matchAll"), F, F, F),
        "replace" -> DataProperty(symbolAddr("replace"), F, F, F),
        "search" -> DataProperty(symbolAddr("search"), F, F, F),
        "species" -> DataProperty(symbolAddr("species"), F, F, F),
        "split" -> DataProperty(symbolAddr("split"), F, F, F),
        "toPrimitive" -> DataProperty(symbolAddr("toPrimitive"), F, F, F),
        "toStringTag" -> DataProperty(symbolAddr("toStringTag"), F, F, F),
        "unscopables" -> DataProperty(symbolAddr("unscopables"), F, F, F),
        "prototype" -> DataProperty(symbolAddr("prototype"), F, F, F),
      ),
    ),
    "Symbol.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Symbol"), T, F, T),
        "@@toPrimitive" ->
        DataProperty(intrAddr("Symbol.prototype[@@toPrimitive]"), F, F, T),
        "@@toStringTag" -> DataProperty(Str("Symbol"), F, F, T),
      ),
    ),
    "Error" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Error.prototype"), F, F, F),
      ),
    ),
    "Error.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Error"), T, F, T),
        "message" -> DataProperty(Str(""), T, F, T),
        "name" -> DataProperty(Str("Error"), T, F, T),
      ),
    ),
    "Number" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "EPSILON" -> DataProperty(Number(math.ulp(1.0)), F, F, F),
        "MAX_SAFE_INTEGER" -> DataProperty(Number(9007199254740991.0), F, F, F),
        "MAX_VALUE" -> DataProperty(Number(Double.MaxValue), F, F, F),
        "MIN_SAFE_INTEGER" ->
        DataProperty(Number(-9007199254740991.0), F, F, F),
        "MIN_VALUE" -> DataProperty(Number(Double.MinPositiveValue), F, F, F),
        "NaN" -> DataProperty(Number(Double.NaN), F, F, F),
        "NEGATIVE_INFINITY" ->
        DataProperty(Number(Double.NegativeInfinity), F, F, F),
        "parseFloat" -> DataProperty(intrAddr("parseFloat"), T, F, T),
        "parseInt" -> DataProperty(intrAddr("parseInt"), T, F, T),
        "POSITIVE_INFINITY" ->
        DataProperty(Number(Double.PositiveInfinity), F, F, F),
        "prototype" -> DataProperty(intrAddr("Number.prototype"), F, F, F),
      ),
    ),
    "Number.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "NumberData" -> Number(0.0),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Number"), T, F, T),
      ),
    ),
    "Number.prototype.toString" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "BigInt" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("BigInt.prototype"), F, F, F),
      ),
    ),
    "BigInt.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("BigInt"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("BigInt"), F, F, T),
      ),
    ),
    "Math" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "E" -> DataProperty(Number(2.7182818284590452354), F, F, F),
        "LN10" -> DataProperty(Number(2.302585092994046), F, F, F),
        "LN2" -> DataProperty(Number(0.6931471805599453), F, F, F),
        "LOG10E" -> DataProperty(Number(0.4342944819032518), F, F, F),
        "LOG2E" -> DataProperty(Number(1.4426950408889634), F, F, F),
        "PI" -> DataProperty(Number(3.1415926535897932), F, F, F),
        "SQRT1_2" -> DataProperty(Number(0.7071067811865476), F, F, F),
        "SQRT2" -> DataProperty(Number(1.4142135623730951), F, F, F),
        "@@toStringTag" -> DataProperty(Str("Math"), F, F, T),
      ),
    ),
    "Math.hypot" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Math.max" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Math.min" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Date" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(7.0), F, F, T),
        "prototype" -> DataProperty(intrAddr("Date.prototype"), F, F, F),
      ),
    ),
    "Date.UTC" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(7.0), F, F, T),
      ),
    ),
    "Date.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Date"), T, F, T),
      ),
    ),
    "Date.prototype.setFullYear" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setHours" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(4.0), F, F, T),
      ),
    ),
    "Date.prototype.setMinutes" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setMonth" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setSeconds" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCFullYear" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCHours" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(4.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCMinutes" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(3.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCMonth" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "Date.prototype.setUTCSeconds" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "String" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("String.prototype"), F, F, F),
      ),
    ),
    "String.fromCharCode" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "String.fromCodePoint" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "String.prototype" -> Struct(
      typeName = "StringExoticObject",
      imap = List(
        "Extensible" -> Bool(true),
        "StringData" -> Str(""),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, F),
        "constructor" -> DataProperty(intrAddr("String"), T, F, T),
      ),
    ),
    "String.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "StringIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("String Iterator"), F, F, T),
      ),
    ),
    "RegExp" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("RegExp.prototype"), F, F, F),
      ),
    ),
    "RegExp.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("RegExp"), T, F, T),
      ),
    ),
    "Array" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" -> DataProperty(intrAddr("Array.prototype"), F, F, F),
      ),
    ),
    "Array.prototype" -> Struct(
      typeName = "Array",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), T, F, F),
        "constructor" -> DataProperty(intrAddr("Array"), T, F, T),
        "@@iterator" ->
        DataProperty(intrAddr("Array.prototype.values"), T, F, T),
        "@@unscopables" ->
        DataProperty(intrAddr("Array.prototype[@@unscopables]"), F, F, T),
      ),
    ),
    "Array.prototype.concat" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.push" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype.unshift" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
      ),
    ),
    "Array.prototype[@@unscopables]" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> Null,
      ),
      nmap = List(
        "at" -> DataProperty(Bool(true), T, T, T),
        "copyWithin" -> DataProperty(Bool(true), T, T, T),
        "entries" -> DataProperty(Bool(true), T, T, T),
        "fill" -> DataProperty(Bool(true), T, T, T),
        "find" -> DataProperty(Bool(true), T, T, T),
        "findIndex" -> DataProperty(Bool(true), T, T, T),
        "findLast" -> DataProperty(Bool(true), T, T, T),
        "findLastIndex" -> DataProperty(Bool(true), T, T, T),
        "flat" -> DataProperty(Bool(true), T, T, T),
        "flatMap" -> DataProperty(Bool(true), T, T, T),
        "includes" -> DataProperty(Bool(true), T, T, T),
        "keys" -> DataProperty(Bool(true), T, T, T),
        "toReversed" -> DataProperty(Bool(true), T, T, T),
        "toSorted" -> DataProperty(Bool(true), T, T, T),
        "toSpliced" -> DataProperty(Bool(true), T, T, T),
        "values" -> DataProperty(Bool(true), T, T, T),
      ),
    ),
    "ArrayIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("Array Iterator"), F, F, T),
      ),
    ),
    "Map" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Map.prototype"), F, F, F),
      ),
    ),
    "Map.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Map"), T, F, T),
        "@@iterator" ->
        DataProperty(intrAddr("Map.prototype.entries"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("Map"), F, F, T),
      ),
    ),
    "MapIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("Map Iterator"), F, F, T),
      ),
    ),
    "Set" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Set.prototype"), F, F, F),
      ),
    ),
    "Set.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Set"), T, F, T),
        "keys" -> DataProperty(intrAddr("Set.prototype.values"), T, F, T),
        "@@iterator" -> DataProperty(intrAddr("Set.prototype.values"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("Set"), F, F, T),
      ),
    ),
    "SetIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("Set Iterator"), F, F, T),
      ),
    ),
    "WeakMap" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("WeakMap.prototype"), F, F, F),
      ),
    ),
    "WeakMap.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("WeakMap"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("WeakMap"), F, F, T),
      ),
    ),
    "WeakSet" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("WeakSet.prototype"), F, F, F),
      ),
    ),
    "WeakSet.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("WeakSet"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("WeakSet"), F, F, T),
      ),
    ),
    "ArrayBuffer" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("ArrayBuffer.prototype"), F, F, F),
      ),
    ),
    "ArrayBuffer.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("ArrayBuffer"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("ArrayBuffer"), F, F, T),
      ),
    ),
    "JSON" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("JSON"), F, F, T),
      ),
    ),
    "JSON.parse" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(2.0), F, F, T),
      ),
    ),
    "JSON.stringify" -> Struct(
      typeName = "BuiltinFunctionObject",
      nmap = List(
        "length" -> DataProperty(Number(3.0), F, F, T),
      ),
    ),
    "Map.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Map"), T, F, T),
        "@@iterator" ->
        DataProperty(intrAddr("Map.prototype.entries"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("Map"), F, F, T),
      ),
    ),
    // XXX REMOVE after update to ES2025
    "IteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
    ),
    "Iterator.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
    ),
    "ForInIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
    ),
    "AsyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
    ),
    "AsyncFromSyncIteratorPrototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
    ),
    "Promise" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(intrAddr("Promise.prototype"), F, F, F),
      ),
    ),
    "Promise.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("Promise"), T, F, T),
        "@@toStringTag" -> DataProperty(Str("Promise"), F, F, T),
      ),
    ),
    "GeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" ->
        DataProperty(intrAddr("GeneratorFunction.prototype"), F, F, F),
      ),
    ),
    "GeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("GeneratorFunction"), F, F, T),
        "prototype" -> DataProperty(
          intrAddr(getGeneratorPrototypeName("", "")),
          F,
          F,
          T,
        ),
        "@@toStringTag" -> DataProperty(Str("GeneratorFunction"), F, F, T),
      ),
    ),
    "AsyncGeneratorFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "prototype" ->
        DataProperty(intrAddr("AsyncGeneratorFunction.prototype"), F, F, F),
      ),
    ),
    "AsyncGeneratorFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" ->
        DataProperty(intrAddr("AsyncGeneratorFunction"), F, F, T),
        "prototype" ->
        DataProperty(
          intrAddr(getGeneratorPrototypeName("Async", "")),
          F,
          F,
          T,
        ),
        "@@toStringTag" -> DataProperty(Str("AsyncGeneratorFunction"), F, F, T),
      ),
    ),
    // Generator.prototype == GeneratorFunction.prototype.prototype
    getGeneratorPrototypeName("", "") -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("IteratorPrototype"),
      ),
      nmap = List(
        "constructor" ->
        DataProperty(intrAddr("GeneratorFunction.prototype"), F, F, T),
        // XXX need to be documented
        "next" -> DataProperty(
          intrAddr(getGeneratorPrototypeName("", ".next")),
          T,
          F,
          T,
        ),
        "@@toStringTag" -> DataProperty(Str("Generator"), F, F, T),
      ),
    ),
    // AsyncGenerator.prototype == AsyncGeneratorFunction.prototype.prototype
    getGeneratorPrototypeName("Async", "") -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("AsyncIteratorPrototype"),
      ),
      nmap = List(
        "constructor" ->
        DataProperty(intrAddr("AsyncGeneratorFunction.prototype"), F, F, T),
        "@@toStringTag" -> DataProperty(Str("AsyncGenerator"), F, F, T),
      ),
    ),
    "AsyncFunction" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Prototype" -> intrAddr("Function"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "length" -> DataProperty(Number(1.0), F, F, T),
        "name" -> DataProperty(Str("AsyncFunction"), F, F, T),
        "prototype" ->
        DataProperty(intrAddr("AsyncFunction.prototype"), F, F, F),
      ),
    ),
    "AsyncFunction.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Function.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("AsyncFunction"), F, F, T),
        "@@toStringTag" -> DataProperty(Str("AsyncFunction"), F, F, T),
      ),
    ),
    "Reflect" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Object.prototype"),
      ),
      nmap = List(
        "@@toStringTag" -> DataProperty(Str("Reflect"), F, F, T),
      ),
    ),
    "Proxy" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
    ),
    "AggregateError" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Error"),
        "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
      ),
      nmap = List(
        "prototype" -> DataProperty(
          intrAddr("AggregateError.prototype"),
          F,
          F,
          F,
        ),
      ),
    ),
    "AggregateError.prototype" -> Struct(
      typeName = "OrdinaryObject",
      imap = List(
        "Extensible" -> Bool(true),
        "Prototype" -> intrAddr("Error.prototype"),
      ),
      nmap = List(
        "constructor" -> DataProperty(intrAddr("AggregateError"), T, F, T),
        "message" -> DataProperty(Str(""), T, F, T),
        "name" -> DataProperty(Str("AggregateError"), T, F, T),
      ),
    ),
    "ThrowTypeError" -> Struct(
      typeName = "BuiltinFunctionObject",
      imap = List(
        "Extensible" -> Bool(false),
      ),
      nmap = List(
        "length" -> DataProperty(Number(0.0), F, F, F),
        "name" -> DataProperty(Str(""), F, F, F),
      ),
    ),
  )

  // error objects
  private def errList: List[String] = List(
    "EvalError",
    "RangeError",
    "ReferenceError",
    "SyntaxError",
    "TypeError",
    "URIError",
  )

  // error intrinsics
  private def errors: Map[String, Struct] = (for {
    name <- errList
    (name, struct) <- getErrorMap(name)
  } yield name -> struct).toMap

  private def getErrorMap(errName: String): Map[String, Struct] =
    Map(
      s"$errName" -> Struct(
        typeName = "BuiltinFunctionObject",
        imap = List(
          "Extensible" -> Bool(true),
          "ScriptOrModule" -> Null,
          "Realm" -> realmAddr,
          INNER_CODE -> intrClo(errName),
          "Prototype" -> intrAddr("Error"),
          "Construct" -> clo("Record[BuiltinFunctionObject].Construct"),
          "InitialName" -> Str(errName),
        ),
        nmap = List(
          "length" -> DataProperty(Number(1.0), F, F, T),
          "name" -> DataProperty(Str(errName), F, F, T),
          "prototype" -> DataProperty(
            intrAddr(s"$errName.prototype"),
            F,
            F,
            F,
          ),
        ),
      ),
      s"$errName.prototype" -> Struct(
        typeName = "OrdinaryObject",
        imap = List(
          "Extensible" -> Bool(true),
          "Prototype" -> intrAddr("Error.prototype"),
        ),
        nmap = List(
          "constructor" -> DataProperty(intrAddr(errName), T, F, T),
          "message" -> DataProperty(Str(""), T, F, T),
          "name" -> DataProperty(Str(errName), T, F, T),
        ),
      ),
    )
}
