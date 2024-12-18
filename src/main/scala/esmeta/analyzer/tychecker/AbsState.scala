package esmeta.analyzer.tychecker

import esmeta.ir.*
import esmeta.ty.{*, given}
import esmeta.ty.util.Stringifier.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.Appender.{*, given}
import esmeta.util.BaseUtils.*

/** abstract states */
trait AbsStateDecl { self: TyChecker =>
  case class AbsState(
    reachable: Boolean,
    locals: Map[Local, AbsValue],
    symEnv: Map[Sym, ValueTy],
    constr: TypeConstr,
  ) extends AbsStateLike {
    import AbsState.*

    given AbsState = this

    /** bottom check */
    def isBottom: Boolean = !reachable

    /** bottom inclusion check */
    def hasBottom: Boolean = locals.values.exists(_.isBottom)

    /** partial order */
    def ⊑(that: AbsState): Boolean = (this, that) match
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            AbsState(_, llocals, lsymEnv, lpred),
            AbsState(_, rlocals, rsymEnv, rpred),
          ) =>
        val TypeConstr(lmap, lexpr, lbot) = lpred
        val TypeConstr(rmap, rexpr, rbot) = rpred
        if (lbot && !rbot) return false
        llocals.forall { (x, lv) =>
          rlocals.get(x).fold(false) { rv =>
            AbsValue.orderHelper(lv, this, rv, that)
          }
        } &&
        lsymEnv.forall { (sym, ty) => rsymEnv.get(sym).fold(false)(ty <= _) } &&
        rmap.forall {
          case (r, (rty, rprov)) =>
            lmap.get(r).fold(false) { (lty, _) => lty <= rty }
        } &&
        rexpr.forall { r => lexpr.fold(false)(_ == r) }

    /** not partial order */
    def !⊑(that: AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom => that
      case _ if that.isBottom => this
      case _ =>
        val (l, r) =
          if (this.constr != that.constr)
            val lxs = this.getImprecBases(that)
            val rxs = that.getImprecBases(this)
            (this.kill(lxs, update = false), that.kill(rxs, update = false))
          else (this, that)
        val newLocals = (for {
          x <- (l.locals.keySet ++ r.locals.keySet).toList
          v = AbsValue.joinHelper(l.get(x), l, r.get(x), r)
        } yield x -> v).toMap
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet ++ r.symEnv.keySet).toList
          ty = l.getTy(sym) || r.getTy(sym)
        } yield sym -> ty).toMap
        val newPred = l.constr || r.constr
        AbsState(true, newLocals, newSymEnv, newPred)

    /** Get imprecise bases. A base is imprecise and should be killed if it is
      * not present in the other state or exists but imprecise than the other
      * state.
      *
      * @param that
      * @return
      */
    def getImprecBases(that: AbsState): Set[SymBase] =
      val locals = (for {
        (x, lv) <- this.locals
        if that.locals.get(x) match
          case None     => true
          case Some(rv) => !(rv.ty(using that) <= lv.ty(using this))
      } yield x).toSet
      val syms = (for {
        (x, lty) <- this.symEnv
        if that.symEnv.get(x) match
          case None      => true
          case Some(rty) => !(rty <= lty)
      } yield x).toSet
      locals ++ syms

    /** meet operator */
    def ⊓(that: AbsState): AbsState = (this, that) match
      case _ if this.isBottom || that.isBottom => Bot
      case (l, r) =>
        val newLocals = (for {
          x <- (l.locals.keySet intersect r.locals.keySet).toList
          v = this.get(x) ⊓ that.get(x)
        } yield x -> v).toMap
        val newSymEnv = (for {
          sym <- (l.symEnv.keySet intersect r.symEnv.keySet).toList
          ty = l.getTy(sym) ⊓ r.getTy(sym)
        } yield sym -> ty).toMap
        val newPred = l.constr && r.constr
        AbsState(true, newLocals, newSymEnv, newPred)

    /** kill bases */
    def kill(bases: Set[SymBase], update: Boolean): AbsState =
      val newLocals = for { (x, v) <- locals } yield x -> v.kill(bases, update)
      val newPred = if (update) constr.kill(bases) else constr
      AbsState(reachable, newLocals, symEnv, newPred)

    /** has imprecise elements */
    def hasImprec: Boolean = locals.values.exists(_.ty.isImprec)

    /** getter */
    def get(x: Var): AbsValue = x match
      case x: Global => base.getOrElse(x, AbsValue.Bot)
      case x: Local  => locals.getOrElse(x, AbsValue.Bot)

    /** getter for symbolic expressions */
    def getTy(expr: SymExpr): ValueTy = {
      import SymExpr.*
      expr match
        case SEBool(b)             => BoolT(b)
        case SERef(ref)            => getTy(ref)
        case SEExists(ref)         => BoolT
        case SETypeCheck(base, ty) => BoolT
        case SETypeOf(base)        => BoolT
        case SEEq(left, right)     => BoolT
        case SEOr(left, right)     => BoolT
        case SEAnd(left, right)    => BoolT
        case SENot(expr)           => BoolT
    }

    /** getter for symbolic references */
    def getTy(ref: SymRef): ValueTy = {
      import SymRef.*
      ref match
        case SBase(x)            => getTy(x)
        case SField(base, field) => get(getTy(base), getTy(field))
    }

    /** getter for symbolic bases */
    def getTy(x: SymBase): ValueTy = x match
      case x: Sym   => symEnv.getOrElse(x, BotT)
      case x: Local => get(x).ty

    def getTy(x: SymTy): ValueTy = x.ty(using this)

    /** getter */
    def get(base: AbsValue, field: AbsValue)(using AbsState): AbsValue = {
      import SymExpr.*, SymRef.*, SymTy.*
      val guard = lookupGuard(base.guard, field)
      (base.symty, field.ty.getSingle) match
        case (SRef(ref), One(Str(f))) =>
          AbsValue(SRef(SField(ref, STy(StrT(f)))), guard)
        case _ =>
          AbsValue(STy(get(base.ty, field.ty)), guard)
    }
    def get(baseTy: ValueTy, fieldTy: ValueTy)(using AbsState): ValueTy =
      lookupAst(baseTy.ast, fieldTy) ||
      lookupStr(baseTy.str, fieldTy) ||
      lookupList(baseTy.list, fieldTy) ||
      lookupRecord(baseTy.record, fieldTy) ||
      lookupMap(baseTy.map, fieldTy)
    // AST lookup
    private def lookupAst(ast: AstTy, field: ValueTy): ValueTy =
      import AstTy.*
      ast match
        case AstTy.Bot => BotT
        case Detail(name, idx) =>
          lookupAstIdxField(name, idx)(field) ||
          lookupAstStrField(field)
        case Simple(names) =>
          if (!field.math.isBottom) AstT // TODO more precise
          else lookupAstStrField(field)
        case _ => AstT

    // lookup index fields of ASTs
    private def lookupAstIdxField(
      name: String,
      idx: Int,
    )(field: ValueTy): ValueTy = field.math.getSingle match
      case Zero => BotT
      case One(k) =>
        (for {
          prod <- cfg.grammar.nameMap.get(name)
          rhs <- prod.rhsVec.lift(idx)
          nt <- rhs.nts.lift(k.toInt)
        } yield AstT(nt.name)).getOrElse(BotT)
      case Many => AstT

    // lookup string fields of ASTs
    private def lookupAstStrField(field: ValueTy): ValueTy =
      val nameMap = cfg.grammar.nameMap
      field.str.getSingle match
        case Zero                               => BotT
        case One(name) if nameMap contains name => AstT(name)
        case _ => AstT // TODO warning(s"invalid access: $name of $ast")

    // string lookup
    private def lookupStr(str: BSet[String], field: ValueTy): ValueTy =
      if (str.isBottom) BotT
      else {
        var res = BotT
        if (field.str contains "length") res ||= NonNegIntT
        if (!field.math.isBottom) res ||= CodeUnitT
        res
      }

    // record lookup
    private def lookupRecord(record: RecordTy, field: ValueTy): ValueTy =
      val str = field.str
      var res = BotT
      def add(fieldStr: String): Unit = res ||= record(fieldStr).value
      if (!record.isBottom) for (fieldStr <- str) add(fieldStr)
      res

    // list lookup
    private def lookupList(list: ListTy, field: ValueTy): ValueTy = list match
      case ListTy.Top        => AnyT
      case ListTy.Elem(elem) => elem
      case ListTy.Bot        => BotT

    // symbol lookup
    private def lookupSymbol(symbol: Boolean, field: ValueTy): ValueTy =
      if (symbol && field.str.contains("Description")) StrT
      else BotT

    // map lookup
    private def lookupMap(map: MapTy, field: ValueTy): ValueTy = map match
      case MapTy.Top              => AnyT
      case MapTy.Elem(key, value) => value
      case MapTy.Bot              => BotT

    // guard lookup
    private def lookupGuard(
      guard: TypeGuard,
      field: AbsValue,
    )(using AbsState): TypeGuard = {
      import RefinementKind.*
      field.ty.str.getSingle match
        case One("Value") =>
          TypeGuard(guard.map.collect {
            case (RefinementKind(ty), map) if ty == NormalT(TrueT) =>
              RefinementKind(TrueT) -> map
            case (RefinementKind(ty), map) if ty == NormalT(FalseT) =>
              RefinementKind(FalseT) -> map
          })
        case _ => TypeGuard.Empty
    }

    /** define variables */
    def define(x: Var, value: AbsValue): AbsState = x match
      case x: Local  => this.update(x, value, refine = false)
      case x: Global => error("do not support defining global variables")

    /** identifier setter */
    def update(x: Var, value: AbsValue, refine: Boolean): AbsState = x match
      case x: Local =>
        val newSt = if (refine) this else this.kill(Set(x), update = true)
        val newV = if (refine) value else value.kill(Set(x), update = true)
        newSt.copy(locals = newSt.locals + (x -> newV), constr = newSt.constr)
      case x: Global => this

    /** type check */
    def typeCheck(value: AbsValue, givenTy: ValueTy): ValueTy =
      val ty = value.ty
      if (ty <= givenTy) TrueT
      else if ((ty && givenTy).isBottom) FalseT
      else BoolT

    /** variable existence check */
    def exists(ref: Ref): AbsValue = AbsValue.BoolTop

    /** expand a field of a record object */
    def expand(base: AbsValue, field: AbsValue): AbsState = this

    /** delete a key from an map object */
    def delete(base: AbsValue, field: AbsValue): AbsState = this

    /** push a value to a list */
    def push(list: AbsValue, value: AbsValue, front: Boolean): AbsState = this

    /** pop a value from a list */
    def pop(list: AbsValue, front: Boolean): (AbsValue, AbsState) =
      (AbsValue(list.ty.list.elem), this)

    /** copy object */
    def copy(from: AbsValue): (AbsValue, AbsState) = (from, this)

    /** get keys of a record/map object as a list */
    def keys(
      base: AbsValue,
      intSorted: Boolean,
    ): (AbsValue, AbsState) =
      val ty = base.ty
      var elemTy = BotT
      if (!ty.record.isBottom) elemTy ||= ty.record.getKey
      if (!ty.map.isBottom) elemTy ||= ty.map.getKey
      if (elemTy.isBottom) (AbsValue.Bot, Bot)
      else (AbsValue(ListT(elemTy)), this)

    /** allocate a record object */
    def allocRecord(
      tname: String,
      pairs: Iterable[(String, AbsValue)] = Nil,
    ): (AbsValue, AbsState) =
      (AbsValue(RecordT(tname, pairs.map(_ -> _.ty).toMap)), this)

    /** allocate a map object */
    def allocMap(
      pairs: Iterable[(AbsValue, AbsValue)] = Nil,
    ): (AbsValue, AbsState) =
      val (keys, values) = pairs.unzip
      val key = keys.foldLeft(BotT)(_ || _.ty)
      val value = values.foldLeft(BotT)(_ || _.ty)
      (AbsValue(MapT(key, value)), this)

    /** allocate a list object */
    def allocList(
      vs: Iterable[AbsValue] = Nil,
    ): (AbsValue, AbsState) =
      (AbsValue(ListT(vs.foldLeft(BotT)(_ || _.ty))), this)
  }
  object AbsState extends DomainLike[AbsState] {

    /** top element */
    lazy val Top: AbsState = exploded("top abstract state")

    /** bottom element */
    lazy val Bot: AbsState = AbsState(false, Map(), Map(), TypeConstr())

    /** empty element */
    lazy val Empty: AbsState = AbsState(true, Map(), Map(), TypeConstr())

    /** appender */
    given rule: Rule[AbsState] = mkRule(true)

    // appender generator
    private def mkRule(detail: Boolean): Rule[AbsState] = (app, elem) =>
      import esmeta.ir.given
      if (!elem.isBottom) {
        val AbsState(reachable, locals, symEnv, pred) = elem
        given localsRule: Rule[Map[Local, AbsValue]] = sortedMapRule(sep = ": ")
        given symEnvRule: Rule[Map[Sym, ValueTy]] = sortedMapRule(sep = ": ")
        given predRule: Rule[Map[SymBase, ValueTy]] =
          sortedMapRule(sep = " <: ")
        if (locals.nonEmpty) app >> locals
        if (symEnv.nonEmpty) app >> symEnv
        app >> pred
        app
      } else app >> "⊥"
  }
}
