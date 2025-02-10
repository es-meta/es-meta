package esmeta.ty

import esmeta.cfg.Func
import esmeta.error.*
import esmeta.error.NotSupported.Category.*
import esmeta.error.NotSupported.{*, given}
import esmeta.state.*
import esmeta.ty.util.Parser
import esmeta.util.*

/** value types */
sealed trait ValueTy extends Ty with Lattice[ValueTy] {
  import ValueTy.*

  def clo: CloTy
  def cont: BSet[Int]
  def record: RecordTy
  def map: MapTy
  def list: ListTy
  def ast: AstTy
  def grammarSymbol: BSet[GrammarSymbol]
  def codeUnit: Boolean
  def enumv: BSet[String]
  def math: MathTy
  def infinity: InfinityTy
  def number: NumberTy
  def bigInt: Boolean
  def str: BSet[String]
  def bool: BoolTy
  def undef: Boolean
  def nullv: Boolean

  /** top check */
  def isTop: Boolean = this eq Top

  /** bottom check */
  def isBottom: Boolean =
    if (this eq Bot) true
    else if (this eq Top) false
    else
      (
        this.clo.isBottom &&
        this.cont.isBottom &&
        this.record.isBottom &&
        this.map.isBottom &&
        this.list.isBottom &&
        this.ast.isBottom &&
        this.grammarSymbol.isBottom &&
        this.codeUnit.isBottom &&
        this.enumv.isBottom &&
        this.math.isBottom &&
        this.infinity.isBottom &&
        this.number.isBottom &&
        this.bigInt.isBottom &&
        this.str.isBottom &&
        this.bool.isBottom &&
        this.undef.isBottom &&
        this.nullv.isBottom
      )

  /** partial order/subset operator */
  def <=(that: => ValueTy): Boolean =
    if ((this eq that) || (this eq Bot) || (that eq Top)) true
    else if (this eq Top) false
    else
      this.clo <= that.clo &&
      this.cont <= that.cont &&
      this.record <= that.record &&
      this.map <= that.map &&
      this.list <= that.list &&
      this.ast <= that.ast &&
      this.grammarSymbol <= that.grammarSymbol &&
      this.codeUnit <= that.codeUnit &&
      this.enumv <= that.enumv &&
      this.math <= that.math &&
      this.infinity <= that.infinity &&
      this.number <= that.number &&
      this.bigInt <= that.bigInt &&
      this.str <= that.str &&
      this.bool <= that.bool &&
      this.undef <= that.undef &&
      this.nullv <= that.nullv

  /** union type */
  def ||(that: => ValueTy): ValueTy =
    if (this eq that) this
    else if (this eq Bot) that
    else if (this eq Top) Top
    else if (that eq Bot) this
    else if (that eq Top) Top
    else
      ValueTy(
        this.clo || that.clo,
        this.cont || that.cont,
        this.record || that.record,
        this.map || that.map,
        this.list || that.list,
        this.ast || that.ast,
        this.grammarSymbol || that.grammarSymbol,
        this.codeUnit || that.codeUnit,
        this.enumv || that.enumv,
        this.math || that.math,
        this.infinity || that.infinity,
        this.number || that.number,
        this.bigInt || that.bigInt,
        this.str || that.str,
        this.bool || that.bool,
        this.undef || that.undef,
        this.nullv || that.nullv,
      ).norm

  /** intersection type */
  def &&(that: => ValueTy): ValueTy =
    if (this eq that) this
    else if (this eq Bot) Bot
    else if (this eq Top) that
    else if (that eq Bot) Bot
    else if (that eq Top) this
    else
      ValueTy(
        this.clo && that.clo,
        this.cont && that.cont,
        this.record && that.record,
        this.map && that.map,
        this.list && that.list,
        this.ast && that.ast,
        this.grammarSymbol && that.grammarSymbol,
        this.codeUnit && that.codeUnit,
        this.enumv && that.enumv,
        this.math && that.math,
        this.infinity && that.infinity,
        this.number && that.number,
        this.bigInt && that.bigInt,
        this.str && that.str,
        this.bool && that.bool,
        this.undef && that.undef,
        this.nullv && that.nullv,
      )

  /** prune type */
  def --(that: => ValueTy): ValueTy =
    if (this eq that) Bot
    else if (this eq Bot) Bot
    else if (that eq Bot) this
    else if (that eq Top) Bot
    else
      ValueTy(
        this.clo -- that.clo,
        this.cont -- that.cont,
        this.record -- that.record,
        this.map -- that.map,
        this.list -- that.list,
        this.ast -- that.ast,
        this.grammarSymbol -- that.grammarSymbol,
        this.codeUnit -- that.codeUnit,
        this.enumv -- that.enumv,
        this.math -- that.math,
        this.infinity -- that.infinity,
        this.number -- that.number,
        this.bigInt -- that.bigInt,
        this.str -- that.str,
        this.bool -- that.bool,
        this.undef -- that.undef,
        this.nullv -- that.nullv,
      )

  /** value containment check */
  def contains(value: Value, heap: Heap): Boolean = value match
    case _ if this eq Top => true
    case a: Addr =>
      heap(a) match
        case obj: RecordObj => record.contains(obj, heap)
        case obj: MapObj    => map.contains(obj, heap)
        case obj: ListObj   => list.contains(obj, heap)
        case obj: YetObj    => true // throw NotSupported(Feature)(obj.msg)
    case Clo(func, captured)             => clo contains func.irFunc.name
    case Cont(func, captured, callStack) => cont contains func.id
    case v: AstValue                     => ast.contains(v)
    case x @ GrammarSymbol(name, params) => grammarSymbol contains x
    case m: Math                         => math contains m
    case Infinity(p)                     => infinity contains p
    case Enum(name)                      => enumv contains name
    case CodeUnit(c)                     => codeUnit
    case n: Number                       => number contains n
    case BigInt(n)                       => bigInt
    case Str(s)                          => str contains s
    case Bool(b)                         => bool contains b
    case Undef                           => undef
    case Null                            => nullv

  /** copy value type */
  def copied(
    clo: CloTy = clo,
    cont: BSet[Int] = cont,
    record: RecordTy = record,
    map: MapTy = map,
    list: ListTy = list,
    ast: AstTy = ast,
    grammarSymbol: BSet[GrammarSymbol] = grammarSymbol,
    codeUnit: Boolean = codeUnit,
    enumv: BSet[String] = enumv,
    math: MathTy = math,
    infinity: InfinityTy = infinity,
    number: NumberTy = number,
    bigInt: Boolean = bigInt,
    str: BSet[String] = str,
    bool: BoolTy = bool,
    undef: Boolean = undef,
    nullv: Boolean = nullv,
  ): ValueTy = ValueTy(
    clo,
    cont,
    record,
    map,
    list,
    ast,
    grammarSymbol,
    codeUnit,
    enumv,
    math,
    infinity,
    number,
    bigInt,
    str,
    bool,
    undef,
    nullv,
  )

  /** completion check */
  def isCompletion: Boolean = this <= CompT

  /** normalization */
  def norm: ValueTy = if (
    clo.isTop &&
    cont.isTop &&
    record.isTop &&
    map.isTop &&
    list.isTop &&
    ast.isTop &&
    grammarSymbol.isTop &&
    codeUnit.isTop &&
    enumv.isTop &&
    math.isTop &&
    infinity.isTop &&
    number.isTop &&
    bigInt.isTop &&
    str.isTop &&
    bool.isTop &&
    undef.isTop &&
    nullv.isTop
  ) ValueTy.Top
  else this

  /** get single value */
  def getSingle: Flat[Value] =
    clo.getSingle ||
    (if (this.cont.isBottom) Zero else Many) ||
    (if (this.record.isBottom) Zero else Many) ||
    (if (this.map.isBottom) Zero else Many) ||
    (if (this.list.isBottom) Zero else Many) ||
    (if (this.ast.isBottom) Zero else Many) ||
    grammarSymbol.getSingle ||
    (if (this.codeUnit.isBottom) Zero else Many) ||
    (enumv.getSingle.map(Enum(_): Value)) ||
    math.getSingle ||
    (infinity.getSingle.map(Infinity(_): Value)) ||
    number.getSingle ||
    (if (this.bigInt.isBottom) Zero else Many) ||
    (str.getSingle.map(Str(_): Value)) ||
    (bool.getSingle.map(Bool(_): Value)) ||
    (if (this.undef.isBottom) Zero else One(Undef)) ||
    (if (this.nullv.isBottom) Zero else One(Null))

  /** single value check */
  def isSingle: Boolean = getSingle match
    case One(_) => true
    case _      => false

  /** types having no field */
  def noField: ValueTy = this match
    case ValueTopTy =>
      ValueElemTy(
        clo = CloTopTy,
        cont = Inf,
        grammarSymbol = Inf,
        codeUnit = true,
        enumv = Inf,
        math = MathTy.Top,
        number = NumberTy.Top,
        bigInt = true,
        bool = BoolTy.Top,
        undef = true,
        nullv = true,
      )
    case elem: ValueElemTy =>
      elem.copy(
        record = RecordTy.Bot,
        map = MapTy.Bot,
        list = ListTy.Bot,
        ast = AstTy.Bot,
        str = Fin(),
      )

  /** boolean operations */
  def and(that: ValueTy): ValueTy = ValueTy.and(this, that)
  def or(that: ValueTy): ValueTy = ValueTy.or(this, that)
  def not: ValueTy = ValueTy.not(this)

  /** type operations */
  def typeOfNames: Set[String] = {
    var names: Set[String] = Set()
    if (!this.number.isBottom) names += "Number"
    if (this.bigInt) names += "BigInt"
    if (!this.str.isBottom) names += "String"
    if (!this.bool.isBottom) names += "Boolean"
    if (this.undef) names += "Undefined"
    if (this.nullv) names += "Null"
    if (!(this && ObjectT).isBottom) names += "Object"
    if (!(this && SymbolT).isBottom) names += "Symbol"
    if (!(this -- ESValueT).isBottom) names += "SpecType"
    names
  }

  /** to list of atomic types */
  def toAtomicTys: List[ValueTy] = this match {
    case ValueTopTy => List(ValueTopTy)
    case ValueElemTy(
          clo,
          cont,
          record,
          map,
          list,
          ast,
          grammarSymbol,
          codeUnit,
          enumv,
          math,
          infinity,
          number,
          bigInt,
          str,
          bool,
          undef,
          nullv,
        ) =>
      var tys: Vector[ValueTy] = Vector()
      tys ++= clo.toAtomicTys.map(clo => ValueElemTy(clo = clo))
      if (!cont.isBottom) tys :+= ValueElemTy(cont = cont)
      tys ++= record.toAtomicTys.map(record => ValueElemTy(record = record))
      if (!map.isBottom) tys :+= ValueElemTy(map = map)
      if (!list.isBottom) tys :+= ValueElemTy(list = list)
      tys ++= ast.toAtomicTys.map(ast => ValueElemTy(ast = ast))
      grammarSymbol match
        case Inf    => tys :+= ValueElemTy(grammarSymbol = Inf)
        case Fin(s) => tys ++= s.map(g => ValueElemTy(grammarSymbol = Fin(g)))
      if (codeUnit) tys :+= ValueElemTy(codeUnit = true)
      enumv match
        case Inf    => tys :+= ValueElemTy(enumv = Inf)
        case Fin(s) => tys ++= s.map(e => ValueElemTy(enumv = Fin(e)))
      tys ++= math.toAtomicTys.map(math => ValueElemTy(math = math))
      if (!infinity.isBottom) tys :+= ValueElemTy(infinity = InfinityTy.Top)
      if (!number.isBottom) tys :+= ValueElemTy(number = NumberTy.Top)
      if (bigInt) tys :+= ValueElemTy(bigInt = true)
      if (!str.isBottom) tys :+= ValueElemTy(str = Inf)
      if (!bool.isBottom) tys :+= ValueElemTy(bool = BoolTy.Top)
      if (undef) tys :+= ValueElemTy(undef = true)
      if (nullv) tys :+= ValueElemTy(nullv = true)
      tys.toList
  }
}

case object ValueTopTy extends ValueTy {
  def clo: CloTy = CloTopTy
  def cont: BSet[Int] = Inf
  def record: RecordTy = RecordTy.Top
  def map: MapTy = MapTy.Top
  def list: ListTy = ListTy.Bot // unsound but need to remove cycle
  def ast: AstTy = AstTy.Top
  def grammarSymbol: BSet[GrammarSymbol] = Inf
  def codeUnit: Boolean = true
  def enumv: BSet[String] = Inf
  def math: MathTy = MathTy.Top
  def infinity: InfinityTy = InfinityTy.Top
  def number: NumberTy = NumberTy.Top
  def bigInt: Boolean = true
  def str: BSet[String] = Inf
  def bool: BoolTy = BoolTy.Top
  def undef: Boolean = true
  def nullv: Boolean = true
}

case class ValueElemTy(
  clo: CloTy = CloTy.Bot,
  cont: BSet[Int] = Fin(),
  record: RecordTy = RecordTy.Bot,
  map: MapTy = MapTy.Bot,
  list: ListTy = ListTy.Bot,
  ast: AstTy = AstTy.Bot,
  grammarSymbol: BSet[GrammarSymbol] = Fin(),
  codeUnit: Boolean = false,
  enumv: BSet[String] = Fin(),
  math: MathTy = MathTy.Bot,
  infinity: InfinityTy = InfinityTy.Bot,
  number: NumberTy = NumberTy.Bot,
  bigInt: Boolean = false,
  str: BSet[String] = Fin(),
  bool: BoolTy = BoolTy.Bot,
  undef: Boolean = false,
  nullv: Boolean = false,
) extends ValueTy
object ValueTy extends Parser.From(Parser.valueTy) {
  def apply(
    clo: CloTy = CloTy.Bot,
    cont: BSet[Int] = Fin(),
    record: RecordTy = RecordTy.Bot,
    map: MapTy = MapTy.Bot,
    list: ListTy = ListTy.Bot,
    ast: AstTy = AstTy.Bot,
    grammarSymbol: BSet[GrammarSymbol] = Fin(),
    codeUnit: Boolean = false,
    enumv: BSet[String] = Fin(),
    math: MathTy = MathTy.Bot,
    infinity: InfinityTy = InfinityTy.Bot,
    number: NumberTy = NumberTy.Bot,
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: BoolTy = BoolTy.Bot,
    undef: Boolean = false,
    nullv: Boolean = false,
  ): ValueTy = ValueElemTy(
    clo,
    cont,
    record,
    map,
    list,
    ast,
    grammarSymbol,
    codeUnit,
    enumv,
    math,
    infinity,
    number,
    bigInt,
    str,
    bool,
    undef,
    nullv,
  ).norm
  lazy val Top: ValueTy = ValueTopTy
  lazy val Bot: ValueTy = ValueElemTy()

  /** get type from `typeof` string */
  def fromTypeOf(str: String): ValueTy = str match
    case "Object"    => RecordT("Object")
    case "Symbol"    => SymbolT
    case "Number"    => NumberT
    case "BigInt"    => BigIntT
    case "String"    => StrT
    case "Boolean"   => BoolT
    case "Undefined" => UndefT
    case "Null"      => NullT
    case _           => Bot

  /** boolean operations */
  type UOp[T] = T => T
  type BOp[T] = (T, T) => T
  private def boolUOp(op: UOp[Boolean]): UOp[ValueTy] = v =>
    BoolT(v.bool.set.map(op))
  private def boolBOp(op: BOp[Boolean]): BOp[ValueTy] = (l, r) =>
    BoolT(for {
      l <- l.bool.set
      r <- r.bool.set
    } yield op(l, r))

  /** boolean operations */
  val and: BOp[ValueTy] = boolBOp(_ && _)
  val or: BOp[ValueTy] = boolBOp(_ || _)
  val not: UOp[ValueTy] = boolUOp(!_)
}
