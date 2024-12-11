package esmeta.analyzer.tychecker

import esmeta.cfg.*
import esmeta.ir.{Func => _, *}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.{*, given}
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** type guards */
trait TypeGuardDecl { self: TyChecker =>

  /** type guard */
  case class TypeGuard(map: Map[RefinementKind, TypeConstr] = Map()) {
    def isEmpty: Boolean = map.isEmpty
    def nonEmpty: Boolean = !isEmpty
    def kinds: Set[RefinementKind] = map.keySet
    def get(kind: RefinementKind): Option[TypeConstr] = map.get(kind)
    def apply(kind: RefinementKind): TypeConstr =
      map.getOrElse(kind, TypeConstr())
    def bases: Set[SymBase] = map.values.flatMap(_.bases).toSet

    def kill(bases: Set[SymBase])(using AbsState): TypeGuard = TypeGuard(for {
      (kind, pred) <- map
      newPred = pred.kill(bases)
      if newPred.nonTop
    } yield kind -> newPred)

    def forReturn(symEnv: Map[Sym, ValueTy]): TypeGuard = TypeGuard(for {
      (kind, pred) <- map
      newPred = pred.forReturn(symEnv)
      if newPred.nonTop
    } yield kind -> newPred)

    def filter(ty: ValueTy): TypeGuard =
      TypeGuard(map.filter { (kind, _) => !(kind.ty && ty).isBottom })

    def has(x: SymBase): Boolean = map.values.exists(_.has(x))

    def <=(that: TypeGuard): Boolean = that.map.forall { (kind, r) =>
      this.map.get(kind) match
        case Some(l) => l <= r
        case None    => false
    }

    def ||(that: TypeGuard)(lty: ValueTy, rty: ValueTy): TypeGuard =
      val (lkinds, rkinds) = (this.kinds, that.kinds)
      val kinds =
        lkinds.filter(k => (k.ty && rty).isBottom || rkinds.contains(k)) ++
        rkinds.filter(k => (k.ty && lty).isBottom || lkinds.contains(k))
      TypeGuard((for {
        kind <- kinds.toList
        pred = (this.get(kind), that.get(kind)) match
          case (Some(l), Some(r)) => l || r
          case (Some(l), None)    => l
          case (None, Some(r))    => r
          case _                  => TypeConstr()
        if !pred.isTop
      } yield kind -> pred).toMap)

    def &&(that: TypeGuard): TypeGuard = TypeGuard((for {
      kind <- (this.kinds ++ that.kinds).toList
      pred = this(kind) && that(kind)
      if !pred.isTop
    } yield kind -> pred).toMap)

    /** Generalize the type guard to unrestricted with the demanded type.
      *
      * @param upperTy
      *   the concrete upper bound type of the associated symbolic type
      * @param ty
      *   type to be checked
      * @return
      *   symbolic predicate ensured when the abstract value is refined to `ty`
      */
    def evaluate(upperTy: ValueTy)(ty: ValueTy): TypeConstr =
      if ((upperTy && ty).isBottom) TypeConstr()
      else {
        val preds = for {
          (kind, pred) <- map
          if ty <= kind.ty
        } yield pred
        if (preds.nonEmpty) preds.reduce(_ && _)
        else TypeConstr()
      }

    override def toString: String = (new Appender >> this).toString
  }

  object TypeGuard {
    val Empty: TypeGuard = TypeGuard()
    def apply(ps: (RefinementKind, TypeConstr)*): TypeGuard = TypeGuard(
      ps.toMap,
    )
  }

  /** type refinement target */
  enum RefinementTarget:
    case BranchTarget(branch: Branch, isTrue: Boolean)
    case AssertTarget(block: Block, idx: Int)
    def node: Node = this match
      case BranchTarget(branch, _) => branch
      case AssertTarget(block, _)  => block
    def func: Func = cfg.funcOf(node)

  case class RefinementKind(private val _ty: ValueTy) {
    def ty: ValueTy = _ty
  }
  object RefinementKind {
    val set: Set[ValueTy] =
      Set(
        TrueT,
        FalseT,
        NormalT,
        AbruptT,
        NormalT(TrueT),
        NormalT(FalseT),
        ENUMT_SYNC,
        ENUMT_ASYNC,
      )

    def apply(ty: ValueTy): RefinementKind =
      if (RefinementKind.set.contains(ty)) new RefinementKind(ty)
      else throw notSupported(s"Unsupported RefinementKind: $ty")

    def from(givenTy: ValueTy): Set[RefinementKind] =
      RefinementKind.set
        .filter(ty => !(givenTy && ty).isBottom)
        .map(RefinementKind(_))
  }

  /** Symbol */
  type Sym = Int
  case class Provenance(map: Map[Func, List[Call]] = Map()) {
    def depth: Int = map.values.map(_.length).max
    def join(that: Provenance): Provenance = Provenance((for {
      key <- (this.map.keySet union that.map.keySet).toList
      calls <- (this.map.get(key), that.map.get(key)) match
        case (Some(lcalls), Some(rcalls)) =>
          Some(if (lcalls.length < rcalls.length) lcalls else rcalls)
        case (Some(lcalls), None) => Some(lcalls)
        case (None, Some(rcalls)) => Some(rcalls)
        case (None, None)         => None
    } yield key -> calls).toMap)
    def forReturn(call: Call): Provenance = Provenance(
      map.map { case (key, calls) => key -> (call :: calls) },
    )
    override def toString: String = (new Appender >> this).toString
  }
  object Provenance {
    def apply(funcs: Func*): Provenance =
      Provenance(funcs.map(_ -> Nil).toMap)
  }

  /** symbolic predicates */
  case class TypeConstr(
    map: Map[SymBase, (ValueTy, Provenance)] = Map(),
    sexpr: Option[(SymExpr, Provenance)] = None,
  ) {
    def isTop: Boolean = map.isEmpty && sexpr.isEmpty
    def nonTop: Boolean = !isTop
    def <=(that: TypeConstr): Boolean =
      that.map.forall {
        case (x, (rty, _)) =>
          this.map.get(x).fold(false) { case (lty, _) => lty <= rty }
      } && (this.sexpr == that.sexpr)
    def ||(that: TypeConstr): TypeConstr = TypeConstr(
      map = (for {
        x <- (this.map.keySet intersect that.map.keySet).toList
        (lty, lprov) = this.map(x)
        (rty, rprov) = that.map(x)
        ty = lty || rty
        prov = lprov join rprov
      } yield x -> (ty, prov)).toMap,
      sexpr = this.sexpr || that.sexpr,
    )
    def &&(that: TypeConstr): TypeConstr = TypeConstr(
      map = (for {
        x <- (this.map.keySet ++ that.map.keySet).toList
        (lty, lprov) = this.map.getOrElse(x, (AnyT, Provenance()))
        (rty, rprov) = that.map.getOrElse(x, (AnyT, Provenance()))
        ty = lty && rty
        prov = lprov join rprov
      } yield x -> (ty, prov)).toMap,
      sexpr = this.sexpr && that.sexpr,
    )
    def has(x: SymBase): Boolean =
      map.contains(x) || sexpr.fold(false) { case (sexpr, _) => sexpr.has(x) }
    def bases: Set[SymBase] =
      map.keySet.collect { case s: Sym => s } ++
      sexpr.fold(Set[SymBase]()) { (sexpr, _) => sexpr.bases }
    def kill(bases: Set[SymBase])(using AbsState): TypeConstr =
      this.copy(
        map.filter { case (x, _) => !bases.contains(x) },
        sexpr.fold(None)((e, p) => e.kill(bases).map(_ -> p)),
      )
    def forReturn(symEnv: Map[Sym, ValueTy]): TypeConstr = TypeConstr(
      map = for {
        case (x: Sym, (ty, prov)) <- map
        origTy = symEnv.getOrElse(x, BotT)
      } yield x -> (origTy && ty, prov),
      sexpr = None,
    )
    def depth: Int =
      val provs = map.values.map(_._2).toList
      sexpr.fold(provs)(_._2 :: provs).map(_.depth).max
    override def toString: String = (new Appender >> this).toString
  }

  object TypeConstr {
    def apply(pair: (SymExpr, Provenance)): TypeConstr =
      TypeConstr(sexpr = Some(pair))
    def apply(pairs: (SymBase, (ValueTy, Provenance))*): TypeConstr =
      TypeConstr(pairs.toMap, None)
  }

  /** symbolic bases */
  type SymBase = Sym | Local

  /** symbolic expressions */
  enum SymExpr {
    case SEBool(b: Boolean)
    case SERef(ref: SymRef)
    case SEExists(ref: SymRef)
    case SETypeCheck(base: SymExpr, ty: ValueTy)
    case SETypeOf(base: SymExpr)
    case SEEq(left: SymExpr, right: SymExpr)
    case SEOr(left: SymExpr, right: SymExpr)
    case SEAnd(left: SymExpr, right: SymExpr)
    case SENot(expr: SymExpr)
    def ||(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that                     => this
      case (SEBool(false), _)                    => that
      case (_, SEBool(false))                    => this
      case (SEBool(true), _) | (_, SEBool(true)) => SEBool(true)
      case _                                     => SEOr(this, that)
    def &&(that: SymExpr): SymExpr = (this, that) match
      case _ if this == that                       => this
      case (SEBool(true), _)                       => that
      case (_, SEBool(true))                       => this
      case (SEBool(false), _) | (_, SEBool(false)) => SEBool(false)
      case _                                       => SEAnd(this, that)
    def has(x: SymBase): Boolean = this match
      case SEBool(b)             => false
      case SERef(ref)            => ref.has(x)
      case SEExists(ref)         => ref.has(x)
      case SETypeCheck(base, ty) => base.has(x)
      case SETypeOf(base)        => base.has(x)
      case SEEq(left, right)     => left.has(x) || right.has(x)
      case SEOr(left, right)     => left.has(x) || right.has(x)
      case SEAnd(left, right)    => left.has(x) || right.has(x)
      case SENot(expr)           => expr.has(x)
    def bases: Set[SymBase] = this match
      case SEBool(b)             => Set()
      case SERef(ref)            => ref.bases
      case SEExists(ref)         => ref.bases
      case SETypeCheck(base, ty) => base.bases
      case SETypeOf(base)        => base.bases
      case SEEq(left, right)     => left.bases ++ right.bases
      case SEOr(left, right)     => left.bases ++ right.bases
      case SEAnd(left, right)    => left.bases ++ right.bases
      case SENot(expr)           => expr.bases
    def kill(bases: Set[SymBase]): Option[SymExpr] = this match
      case SEBool(b)             => Some(this)
      case SERef(ref)            => ref.kill(bases).map(SERef(_))
      case SEExists(ref)         => ref.kill(bases).map(SEExists(_))
      case SETypeCheck(base, ty) => base.kill(bases).map(SETypeCheck(_, ty))
      case SETypeOf(base)        => base.kill(bases).map(SETypeOf(_))
      case SEEq(left, right) =>
        for {
          l <- left.kill(bases)
          r <- right.kill(bases)
        } yield SEEq(l, r)
      case SEOr(left, right) =>
        for {
          l <- left.kill(bases)
          r <- right.kill(bases)
        } yield SEOr(l, r)
      case SEAnd(left, right) =>
        (left.kill(bases), right.kill(bases)) match
          case (Some(l), Some(r)) => Some(l && r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
      case SENot(expr) => expr.kill(bases).map(SENot(_))
    override def toString: String = (new Appender >> this).toString
  }
  object SymExpr {
    val T: SymExpr = SEBool(true)
    val F: SymExpr = SEBool(false)
    extension (l: Option[(SymExpr, Provenance)])
      def &&(
        r: Option[(SymExpr, Provenance)],
      ): Option[(SymExpr, Provenance)] = (l, r) match
        case (Some(le, lp), Some(re, rp)) => Some(le && re, lp join rp)
        case (Some(l), None)              => Some(l)
        case (None, Some(r))              => Some(r)
        case _                            => None
      def ||(
        r: Option[(SymExpr, Provenance)],
      ): Option[(SymExpr, Provenance)] = (l, r) match
        case (Some(le, lp), Some(re, rp)) => Some(le || re, lp join rp)
        case _                            => None
  }

  /** Symbolic reference
    *
    * This is a path object which makes available to refine a canonical local
    * variable or a symbol, which are represented by `SymBase`.
    */
  enum SymRef {
    case SBase(base: SymBase)
    case SField(base: SymRef, field: SymTy)
    def getBase: SymBase = this match
      case SBase(s)        => s
      case SField(base, f) => base.getBase
    def has(x: SymBase): Boolean = this match
      case SBase(y)        => x == y
      case SField(base, f) => base.has(x) || f.has(x)
    def bases: Set[SymBase] = this match
      case SBase(x)        => Set(x)
      case SField(base, f) => base.bases ++ f.bases
    def kill(bases: Set[SymBase]): Option[SymRef] = this match
      case SBase(x) => if (bases contains x) None else Some(this)
      case SField(base, f) =>
        for {
          b <- base.kill(bases)
          f <- f.kill(bases)
        } yield SField(b, f)
    override def toString: String = (new Appender >> this).toString
  }

  // -----------------------------------------------------------------------------
  // helpers
  // -----------------------------------------------------------------------------
  import TyStringifier.given
  val irStringifier = IRElem.getStringifier(true, false)
  import irStringifier.given
  given Rule[SymBase] = (app, base) =>
    base match
      case sym: Sym     => app >> "#" >> sym.toString
      case local: Local => app >> local.toString
  given Ordering[SymBase] = Ordering.by(_.toString)
  given Rule[SymExpr] = (app, expr) =>
    import SymExpr.*
    expr match
      case SEBool(bool)  => app >> bool
      case SERef(ref)    => app >> ref
      case SEExists(ref) => app >> "(exists " >> ref >> ")"
      case SETypeCheck(expr, ty) =>
        app >> "(? " >> expr >> ": " >> ty >> ")"
      case SETypeOf(base) =>
        app >> "(typeof " >> base >> ")"
      case SEEq(left, right) =>
        app >> "(=" >> " " >> left >> " " >> right >> ")"
      case SEOr(left, right) =>
        app >> "(|| " >> left >> " " >> right >> ")"
      case SEAnd(left, right) =>
        app >> "(&& " >> left >> " " >> right >> ")"
      case SENot(expr) =>
        app >> "(! " >> expr >> ")"
  given Rule[SymRef] = (app, ref) =>
    import SymExpr.*, SymRef.*, SymTy.*
    lazy val inlineField = "([_a-zA-Z][_a-zA-Z0-9]*)".r
    ref match
      case SBase(x) => app >> x
      case SField(base, STy(x)) if !x.isBottom =>
        x.getSingle match
          case One(f: String) => app >> base >> "." >> f
          case _              => app >> base >> "[" >> x >> "]"
      case SField(base, field) => app >> base >> "[" >> field >> "]"
  given Rule[Provenance] = (app, prov) =>
    val Provenance(map) = prov
    if (map.nonEmpty) (app >> " <from> ").wrap("{", "}") {
      for ((func, calls) <- map.toList.sortBy(_._1)) {
        app :> func.nameWithId
        if (calls.nonEmpty) {
          for (call <- calls.reverse)
            app :> "<- " >> call.name >> " @ " >> cfg.funcOf(call).nameWithId
        }
      }
    }
    app
  given Rule[TypeConstr] = (app, pred) =>
    import TypeConstr.*
    given Rule[(ValueTy, Provenance)] =
      case (app, (ty, prov)) => app >> ty >> prov
    given Rule[Map[SymBase, (ValueTy, Provenance)]] = sortedMapRule(sep = ": ")
    if (pred.map.nonEmpty) app >> pred.map
    pred.sexpr.fold(app) { (sexpr, prov) => app >> sexpr >> prov }
  given Rule[TypeGuard] = (app, guard) =>
    given Ordering[RefinementKind] = Ordering.by(_.toString)
    given Rule[RefinementKind] = (app, kind) => app >> kind.ty
    given Rule[Map[RefinementKind, TypeConstr]] =
      sortedMapRule("{", "}", " => ")
    app >> guard.map
  given Rule[RefinementTarget] = (app, target) =>
    import RefinementTarget.*
    val node = target.node
    val func = target.func
    app >> func.nameWithId >> ":" >> node.name >> ":"
    target match
      case BranchTarget(branch, isTrue) =>
        app >> (if (isTrue) "T" else "F")
      case AssertTarget(block, idx) =>
        app >> idx
  given Ordering[RefinementTarget] = Ordering.by { target =>
    import RefinementTarget.*
    target match
      case BranchTarget(branch, isTrue) => (branch.id, if (isTrue) 1 else 0)
      case AssertTarget(block, idx)     => (block.id, idx)
  }
}
