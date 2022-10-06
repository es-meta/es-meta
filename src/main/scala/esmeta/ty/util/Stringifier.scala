package esmeta.ty.util

import esmeta.LINE_SEP
import esmeta.state.Number
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for types */
object Stringifier {

  /** type elements */
  given elemRule: Rule[TyElem] = (app, elem) =>
    elem match
      case elem: UnknownTy   => unknownTyRule(app, elem)
      case elem: ValueTy     => valueTyRule(app, elem)
      case elem: CompTy      => compTyRule(app, elem)
      case elem: ListTy      => listTyRule(app, elem)
      case elem: PureValueTy => pureValueTyRule(app, elem)
      case elem: NameTy      => nameTyRule(app, elem)
      case elem: RecordTy    => recordTyRule(app, elem)
      case elem: AstValueTy  => astValueTyRule(app, elem)
      case elem: SubMapTy    => subMapTyRule(app, elem)

  /** types */
  given tyRule: Rule[Ty] = (app, ty) =>
    ty match
      case ty: UnknownTy => unknownTyRule(app, ty)
      case ty: ValueTy   => valueTyRule(app, ty)

  /** unknown types */
  given unknownTyRule: Rule[UnknownTy] = (app, ty) =>
    app >> "Unknown"
    ty.msg.fold(app)(app >> "[\"" >> _ >> "\"]")

  /** value types */
  given valueTyRule: Rule[ValueTy] = (app, ty) =>
    if (!ty.isBottom)
      FilterApp(app)
        .add(ty.comp, !ty.comp.isBottom)
        .add(ty.pureValue, !ty.pureValue.isBottom)
        .add(ty.subMap, !ty.subMap.isBottom)
        .app
    else app >> "Bot"

  /** completion record types */
  given compTyRule: Rule[CompTy] = (app, ty) =>
    given Rule[Option[PureValueTy]] = topRule
    FilterApp(app)
      .add(ty.normal, !ty.normal.fold(false)(_.isBottom), "Normal")
      .add("Abrupt", !ty.abrupt.isBottom)
      .app

  /** list types */
  given listTyRule: Rule[ListTy] = (app, ty) =>
    ty.elem match
      case None => app
      case Some(elem) =>
        if (elem.isBottom) app >> "Nil"
        else app >> "List[" >> elem >> "]"

  /** pure value types (non-completion record types) */
  given pureValueTyRule: Rule[PureValueTy] = (app, ty) =>
    if (ty == ESPureValueT) app >> "ESValue"
    else
      FilterApp(app)
        .add(ty.clo.map(s => s"\"$s\""), !ty.clo.isBottom, "Clo")
        .add(ty.cont, !ty.cont.isBottom, "Cont")
        .add(ty.name, !ty.name.isBottom)
        .add(ty.record, !ty.record.isBottom)
        .add(ty.list, !ty.list.isBottom)
        .add("Symbol", !ty.symbol.isBottom)
        .add(ty.astValue, !ty.astValue.isBottom)
        .add(ty.nt.map(_.toString), !ty.nt.isBottom, "Nt")
        .add("CodeUnit", !ty.codeUnit.isBottom)
        .add(ty.const.map(s => s"~$s~"), !ty.const.isBottom, "Const")
        .add(ty.math, !ty.math.isBottom, "Math")
        .add(ty.number, !ty.number.isBottom, "Number")
        .add("BigInt", !ty.bigInt.isBottom)
        .add(ty.str.map(s => s"\"$s\""), !ty.str.isBottom, "String")
        .add(ty.bool, !ty.bool.isBottom)
        .add("Undefined", !ty.undef.isBottom)
        .add("Null", !ty.nullv.isBottom)
        .add("Absent", !ty.absent.isBottom)
        .app

  /** named record types */
  given nameTyRule: Rule[NameTy] = (app, ty) =>
    given Rule[Iterable[String]] = iterableRule(sep = OR)
    app >> ty.set.toList.sorted

  /** record types */
  given recordTyRule: Rule[RecordTy] = (app, ty) =>
    given Rule[(String, Option[ValueTy])] = {
      case (app, (key, value)) =>
        app >> "[[" >> key >> "]]"
        value.fold(app)(app >> ": " >> _)
    }
    given Rule[List[(String, Option[ValueTy])]] = iterableRule("{ ", ", ", " }")
    app >> ty.map.toList.sortBy(_._1)

  /** AST value types */
  given astValueTyRule: Rule[AstValueTy] = (app, ty) =>
    app >> "Ast"
    ty match
      case AstTopTy         => app
      case AstNameTy(names) => app >> names
      case AstSingleTy(x, i, j) =>
        app >> ":" >> x >> "[" >> i >> "," >> j >> "]"

  /** sub map types */
  given subMapTyRule: Rule[SubMapTy] = (app, ty) =>
    app >> "SubMap[" >> ty.key >> " |-> " >> ty.value >> "]"

  // rule for bounded set lattice
  private given bsetRule[T: Ordering](using Rule[T]): Rule[BSet[T]] =
    (app, set) =>
      given Rule[List[T]] = iterableRule("[", ", ", "]")
      set match
        case Inf      => app
        case Fin(set) => app >> set.toList.sorted

  // rule for string set
  private given setRule[T: Ordering](using Rule[T]): Rule[Set[T]] =
    (app, set) =>
      given Rule[List[T]] = iterableRule("[", ", ", "]")
      app >> set.toList.sorted

  // rule for boolean set
  private given boolSetRule: Rule[Set[Boolean]] = (app, set) =>
    set.toList match
      case Nil        => app
      case List(bool) => app >> (if (bool) "True" else "False")
      case _          => app >> "Boolean"

  // rule for option type for top
  private def topRule[T](using Rule[T]): Rule[Option[T]] = (app, opt) =>
    opt.fold(app)(app >> "[" >> _ >> "]")

  // appender with filtering
  private class FilterApp(val app: Appender) {
    private var first = true
    def add[T](
      t: => T,
      valid: Boolean,
      pre: String = "",
      post: String = "",
    )(using tRule: Rule[T]): this.type =
      if (valid)
        if (!first) app >> OR
        else first = false
        app >> pre >> t >> post
      this
  }

  // rule for number
  private given numberRule: Rule[Number] = (app, number) =>
    number match
      case Number(Double.PositiveInfinity) => app >> "+INF"
      case Number(Double.NegativeInfinity) => app >> "-INF"
      case Number(n) if n.isNaN            => app >> "NaN"
      case Number(n)                       => app >> n
  given Ordering[Number] = Ordering.by(_.n)

  // separator for type disjuction
  private val OR = " | "
}
