package esmeta.analyzer.tychecker

import esmeta.cfg.{util => _, *}
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.*
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec
import esmeta.es.builtin.JOB_QUEUE

trait AbsTransferDecl { analyzer: TyChecker =>

  /** abstract transfer function */
  class AbsTransfer extends AbsTransferLike {

    /** loading monads */
    import monad.*

    /** loading predefined abstract values */
    import AbsValue.*

    /** loading constructors */
    import SymExpr.*, SymRef.*, SymTy.*

    // =========================================================================
    // Implementation for General AbsTransfer
    // =========================================================================
    /** transfer function for node points */
    def apply(np: NodePoint[_]): Unit =
      // record current control point for alarm
      val st = getState(np)
      given NodePoint[_] = np
      given AbsState = st
      val NodePoint(func, node, view) = np
      node match
        case Block(_, insts, next) =>
          val newSt = insts.zipWithIndex.foldLeft(st) {
            case (nextSt, _) if nextSt.isBottom => nextSt
            case (nextSt, (inst, idx))          => transfer(inst, idx)(nextSt)
          }
          next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case call: Call =>
          val (_, newSt) = (for {
            v <- transfer(call)
            _ <-
              if (v.isBottom) put(AbsState.Bot)
              else modify(_.define(call.lhs, v))
          } yield ())(st)
          call.next.foreach(to => analyzer += getNextNp(np, to) -> newSt)
        case br @ Branch(_, kind, c, thenNode, elseNode) =>
          import RefinementTarget.*
          import RefinementKind.*
          (for { v <- transfer(c); newSt <- get } yield {
            if (v.ty.bool.contains(true))
              val rst = refine(c, v, TrueT)(newSt)
              thenNode.map(analyzer += getNextNp(np, _) -> rst)
              if (detail)
                val constr = v.guard.get(RefinementKind(TrueT))
                logRefined(BranchTarget(br, true), constr, newSt, rst)
            if (v.ty.bool.contains(false))
              val rst = refine(c, v, FalseT)(newSt)
              elseNode.map(analyzer += getNextNp(np, _) -> rst)
              if (detail)
                val constr = v.guard.get(RefinementKind(FalseT))
                logRefined(BranchTarget(br, false), constr, newSt, rst)
          })(st)

    def logRefined(
      target: RefinementTarget,
      constr: Option[TypeConstr],
      st: AbsState,
      refinedSt: AbsState,
    ): Unit =
      val xs = for {
        (x, v) <- st.locals
        ty = v.ty(using st)
        refinedTy = refinedSt.get(x).ty(using refinedSt)
        if refinedTy != ty
      } yield x
      if (xs.isEmpty) refined -= target
      else refined += target -> (xs.toSet, constr.fold(0)(_.depth))

    /** Refine the state based on the given expression, evaluated value, and the
      * required type.
      *
      * @param expr
      *   an expression
      * @param v
      *   an abstract value evaluated from the `expr`
      * @param ty
      *   a required type
      * @return
      *   an updater
      */
    def refine(
      expr: Expr,
      v: AbsValue,
      ty: ValueTy,
    )(using st: AbsState, np: NodePoint[_]): Updater = st =>
      import RefinementKind.*
      val kind = RefinementKind(ty)
      if (inferTypeGuard) {
        // This additional guard is inferred here because of the performance reason
        val newGuard = (for {
          ref <- toSymRef(expr, v)
          pair <- reduceToBase(ref -> kind.ty, np)
        } yield TypeGuard(kind -> TypeConstr(pair))).fold(v.guard) { guard =>
          v.guard && guard
        }
        refine(v.guard.evaluate(v.ty)(kind.ty))(st)
      } else {
        v.guard.get(kind) match
          case Some(constr) => refine(constr)(st) // for default type guards
          case None =>
            ty match // syntactic refinement
              case TrueT  => syntacticRefine(expr, true)(st)
              case FalseT => syntacticRefine(expr, false)(st)
              case _      => throw new Exception(s"Unsupported type: $ty")
      }

    /** get next node point */
    def getNextNp(fromCp: NodePoint[Node], to: Node): NodePoint[Node] =
      fromCp.copy(node = to)

    /** transfer function for return points */
    def apply(rp: ReturnPoint): Unit = if (!canUseReturnTy(rp.func)) {
      var AbsRet(value) = getState(rp)
      for {
        callerNps <- retEdges.get(rp)
        callerNp <- callerNps
        nextNp <- getAfterCallNp(callerNp)
      } {
        given callerSt: AbsState = callInfo(callerNp)
        val retTy = rp.func.retTy.ty.toValue
        val newV = instantiateCall(value, callerNp) ⊓ AbsValue(retTy)
        val nextSt = callerSt.update(callerNp.node.lhs, newV, refine = false)
        analyzer += nextNp -> nextSt
      }
    }

    /** get after call node point */
    def getAfterCallNp(callerNp: NodePoint[Call]): Option[NodePoint[Node]] =
      callerNp.node.next.map(nextNode => callerNp.copy(node = nextNode))

    /** transfer function for call instructions */
    def transfer(
      call: Call,
    )(using np: NodePoint[_]): Result[AbsValue] = {
      val callerNp = NodePoint(np.func, call, np.view)
      call.callInst match {
        case ICall(_, fexpr, args) =>
          for {
            fv <- transfer(fexpr)
            st <- get
            given AbsState = st
            fty = fv.ty
            vs <- join(args.map(transfer))
          } yield {
            val cloRes = fty.clo match
              case CloTopTy           => AnyT
              case CloArrowTy(_, ret) => ret
              case CloSetTy(names) =>
                for {
                  fname <- names
                  f <- cfg.fnameMap.get(fname)
                } {
                  val callPoint = CallPoint(callerNp, f)
                  val captured: Map[Name, AbsValue] = Map() // TODO
                  doCall(callPoint, st, args, vs, captured, f.isMethod)
                }
                BotT
            val contRes = fty.cont match
              case Inf => AnyT
              case Fin(fids) =>
                for {
                  fid <- fty.cont.toIterable(stop = false)
                  f <- cfg.funcMap.get(fid)
                  view = if (typeSens) View(vs.map(_.ty)) else emptyView
                  tgt = Some(NodePoint(f, f.entry, view))
                } {
                  val callPoint = CallPoint(callerNp, f)
                  val captured: Map[Name, AbsValue] = Map() // TODO
                  doCall(callPoint, st, args, vs, captured, f.isMethod, tgt)
                }
                BotT
            AbsValue(cloRes || contRes)
          }
        case ISdoCall(_, base, method, args) =>
          for {
            bv <- transfer(base)
            vs <- join(args.map(transfer))
            st <- get
            given AbsState = st
            bty = bv.ty
          } yield {
            var newV: AbsValue = AbsValue.Bot
            // lexical sdo
            newV ⊔= bv.getLexical(method)

            // syntactic sdo
            for ((sdo, ast) <- bv.getSdo(method))
              val callPoint = CallPoint(callerNp, sdo)
              doCall(callPoint, st, base :: args, ast :: vs, method = true)

            newV
          }
      }
    }

    /** handle calls */
    def doCall(
      callPoint: CallPoint,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      contTarget: Option[NodePoint[Node]] = None,
    ): Unit = {
      val CallPoint(callerNp, callee) = callPoint
      given AbsState = callerSt
      callInfo += callerNp -> callerSt
      val argsInfo = (args zip vs)
      analyzer.argsInfo += callerNp -> argsInfo
      if (canUseReturnTy(callee)) {
        val call = callerNp.node
        val retTy = callee.retTy.ty.toValue
        val newRetV = (for {
          refiner <- manualRefiners.get(callee.name)
          v = refiner(callee, vs, retTy, callerSt)
          newV = instantiateCall(v, callerNp)
        } yield newV).getOrElse {
          val v = AbsValue(retTy)
          v.addGuard(v.genTypeGuard)
        }
        for {
          nextNp <- getAfterCallNp(callerNp)
          newSt = callerSt.define(call.lhs, newRetV)
        } analyzer += nextNp -> newSt
      }
      // get locals
      val locals = getLocals(callPoint, method, vs) ++ captured
      // keep caller state to restore it
      contTarget match
        case Some(target) =>
          analyzer += target -> getCalleeState(callerSt, locals)
        case None =>
          for {
            (calleeView, newLocals) <- getCalleeEntries(callerNp, locals)
            calleeSt = getCalleeState(callerSt, newLocals)
            calleeNp = NodePoint(callee, callee.entry, calleeView)
          } {
            // add callee to worklist
            analyzer += calleeNp -> calleeSt
            // add return edges from callee to caller
            val rp = ReturnPoint(callee, calleeNp.view)
            val set = retEdges.getOrElse(rp, Set())
            retEdges += rp -> (set + callerNp)
            // propagate callee analysis result
            propagate(rp, callerNp)
          }
    }

    /** Returns a symbolic reference which can be refined if the `expr`
      * evaluates to the `value`.
      *
      * @param expr
      *   an expression
      * @param value
      *   an abstract value evaluated from the `expr`
      * @return
      *   a symbolic reference if it can be refined, otherwise `None`
      */
    def toSymRef(expr: Expr, value: AbsValue): Option[SymRef] =
      value.symty match
        case SRef(ref) => Some(ref)
        case _ =>
          expr match
            case ERef(ref) => toSymRef(ref)
            case _         => None

    /** Returns an according symbolic reference when the IR Reference if given.
      *
      * @param ref
      *   an IR reference
      * @param value
      *   an abstract value evaluated from the `ref`
      * @return
      *   a symbolic reference if it can be refined, otherwise `None`
      */
    def toSymRef(ref: Ref, value: AbsValue): Option[SymRef] =
      value.symty match
        case SRef(ref) => Some(ref)
        case _         => toSymRef(ref)

    /** Returns a symbolic reference which can be refined when the reference is
      * given.
      *
      * @param ref
      *   an IR reference
      * @return
      *   a symbolic reference if it can be refined, otherwise `None`
      */
    private def toSymRef(ref: Ref): Option[SymRef] = ref match
      case x: Local => Some(SBase(x))
      case Field(base, EStr(field)) =>
        for {
          b <- toSymRef(base)
        } yield SField(b, STy(StrT(field)))
      case _ => None

    /** get local variables */
    def getLocals(
      callPoint: CallPoint,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] = {
      val CallPoint(callerNp, callee) = callPoint
      val arity @ (from, to) = callee.arity
      val len = vs.length
      if (config.checkArity && (len < from || to < len))
        addError(ArityMismatch(callPoint, len))
      // get parameters
      val params: List[Param] = callee.irFunc.params
      // full arguments with optional parameters
      // construct local type environment
      (for {
        ((param, arg), idx) <- (params zip vs).zipWithIndex
      } yield param.lhs -> assignArg(callPoint, method, idx, param, arg))
    }

    /** assign argument to parameter */
    def assignArg(
      callPoint: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue =
      given AbsState = getState(callPoint.callerNp)
      val paramTy = param.ty.ty.toValue
      val argTy = arg.ty
      if (method && idx == 0) () /* ignore `this` for method calls */
      else if (config.checkParamType && !(argTy <= paramTy))
        addError(ParamTypeMismatch(ArgAssignPoint(callPoint, idx), argTy))
      AbsValue(paramTy && argTy)

    /** callee entries */
    def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] = {
      given AbsState = getState(callerNp)
      if (typeSens) {
        val tys = locals.map { (_, value) => value.ty }
        val xs = locals.map { (x, _) => x }
        for {
          ts <- toAtomic(tys)
          view = View(ts)
          newLocals = xs zip ts.map(AbsValue(_))
        } yield view -> newLocals
      } else List(emptyView -> locals.map { (x, v) => x -> AbsValue(v.ty) })
    }

    def toAtomic(tys: List[ValueTy]): List[List[ValueTy]] = tys match
      case Nil => List(Nil)
      case head :: tail =>
        val tails = toAtomic(tail)
        for {
          h <- head.toAtomicTys
          t <- tails
        } yield h :: t

    /** propagate callee analysis result */
    def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit = {
      if (!canUseReturnTy(rp.func)) {
        val AbsRet(value) = getState(rp)
        (for {
          nextNp <- getAfterCallNp(callerNp)
          callerSt = callInfo(callerNp)
          given AbsState = callerSt
          retTy = rp.func.retTy.ty.toValue
          newV = instantiateCall(value, callerNp) ⊓ AbsValue(retTy)
          if !newV.isBottom
        } yield analyzer += nextNp -> callerSt.define(callerNp.node.lhs, newV))
          .getOrElse {
            if (!getState(rp).isBottom) worklist += rp
          }
      }
    }

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
    )(using np: NodePoint[_]): Updater = transfer(inst, -1)

    /** transfer function for normal instructions */
    def transfer(
      inst: NormalInst,
      idx: Int,
    )(using np: NodePoint[_]): Updater = inst match {
      case IExpr(expr) =>
        for {
          v <- transfer(expr)
          _ <- if (v.isBottom) put(AbsState.Bot) else pure(())
        } yield ()
      case ILet(id, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.define(id, v))
          st <- get
        } yield ()
      case IAssign(x: Local, expr) =>
        for {
          v <- transfer(expr)
          _ <- modify(_.update(x, v, refine = false))
        } yield ()
      case IAssign(Field(x: Var, EStr(f)), expr) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
          ty <- get(_.get(x).ty)
          record = ty.record.update(f, v.ty, refine = false)
          _ <- modify(
            _.update(x, AbsValue(ty.copied(record = record)), refine = false),
          )
        } yield ()
      case IAssign(ref, expr)  => st => st /* TODO */
      case IExpand(base, expr) => st => st /* TODO */
      case IDelete(base, expr) => st => st /* TODO */
      case IPush(expr, ERef(list: Local), _) =>
        for {
          given AbsState <- get
          l <- transfer(list)
          v <- transfer(expr)
          elem = l.ty.list.elem || v.ty
          newV = AbsValue(ListT(elem))
          _ <- modify(_.update(list, newV, refine = false))
        } yield ()
      case IPush(expr, list, _) => st => st /* TODO */
      case IPop(lhs, list, front) =>
        for {
          v <- transfer(list)
          pv <- id(_.pop(v, front))
          _ <- modify(_.define(lhs, pv))
        } yield ()
      case inst @ IReturn(expr) =>
        for {
          v <- transfer(expr)
          _ <- doReturn(inst, v)
          _ <- put(AbsState.Bot)
        } yield ()
      case IAssert(expr: EYet) =>
        st => st /* skip not yet compiled assertions */
      case IAssert(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          constr = v.guard.get(RefinementKind(TrueT))
          _ <- modify(refine(expr, v, TrueT)(using st))
          refinedSt <- get
          given AbsState = refinedSt
          _ = if (detail) np.node match
            case block: Block =>
              logRefined(
                RefinementTarget.AssertTarget(block, idx),
                constr,
                st,
                refinedSt,
              )
            case _ =>
          _ <- if (v ⊑ False) put(AbsState.Bot) else pure(())
        } yield ()
      case IPrint(expr) => st => st /* skip */
      case INop()       => st => st /* skip */
    }

    /** Handle return instructions.
      *
      * @param irReturn
      *   an IR return instruction
      * @param v
      *   an abstract value to return
      * @param np
      *   current node point
      */
    def doReturn(
      irReturn: Return,
      v: AbsValue,
    )(using np: NodePoint[Node]): Unit =
      val NodePoint(func, _, view) = np

      val givenSt = getState(np)
      val irp = InternalReturnPoint(np, irReturn)

      val entryView = getEntryView(view)
      val entryNp = NodePoint(func, func.entry, entryView)
      val entrySt = getState(entryNp)
      given AbsState = entrySt

      val givenV = v.forReturn(givenSt, func, entrySt)
      val rp = ReturnPoint(func, entryView)

      val newV = func.retTy.ty match
        case _: UnknownTy        => givenV
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          val givenTy = givenV.ty(using entrySt)
          if (givenTy <= expectedTy) givenV
          else
            if (config.checkReturnType)
              addError(ReturnTypeMismatch(irp, givenTy))
            AbsValue(STy(givenTy && expectedTy), givenV.guard)
      if (!newV.isBottom)
        val AbsRet(oldV) = getState(rp)
        if (!oldV.isBottom && useRepl) Repl.merged = true
        if (newV !⊑ oldV)
          val v = (oldV ⊔ newV)
          rpMap += rp -> AbsRet(v.forReturn(entrySt))
          worklist += rp

    /** transfer function for expressions */
    def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = st => {
      val (v, newSt) = (for {
        v <- basicTransfer(expr)
        given AbsState <- get
        guard <- if (inferTypeGuard) getTypeGuard(expr) else pure(TypeGuard())
        newV = if (inferTypeGuard) v.addGuard(guard) else v
      } yield newV)(st)
      // No propagation if the result of the expression is bottom
      if (v.isBottom) (v, AbsState.Bot) else (v, newSt)
    }

    /** transfer function for expressions */
    def basicTransfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match {
      case EParse(code, rule) =>
        for {
          c <- transfer(code)
          r <- transfer(rule)
          given AbsState <- get
        } yield c.parse(r)
      case EGrammarSymbol(name, params) =>
        val s = GrammarSymbol(name, params)
        AbsValue(GrammarSymbolT(s))
      case ESourceText(expr) =>
        for {
          v <- transfer(expr)
        } yield StrTop
      case EYet(msg) =>
        if (yetThrow) notSupported(msg)
        else AbsValue.Bot
      case EContains(list, elem) =>
        for {
          l <- transfer(list)
          v <- transfer(elem)
          given AbsState <- get
        } yield
          if (l.ty.list.isBottom) AbsValue.Bot
          else BoolTop
      case ESubstring(expr, from, None) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
        } yield v.substring(f)
      case ESubstring(expr, from, Some(to)) =>
        for {
          v <- transfer(expr)
          f <- transfer(from)
          t <- transfer(to)
        } yield v.substring(f, t)
      case ETrim(expr, isStarting) =>
        for {
          v <- transfer(expr)
        } yield v.trim(isStarting)
      case ERef(ref) =>
        for {
          v <- transfer(ref)
        } yield v
      case unary @ EUnary(_, expr) =>
        for {
          v <- transfer(expr)
          st <- get
          v0 <- transfer(st, unary, v)
        } yield v0
      case binary @ EBinary(BOp.And | BOp.Or, left, right) =>
        shortCircuit(binary, left, right)
      case binary @ EBinary(_, left, right) =>
        for {
          lv <- transfer(left)
          rv <- transfer(right)
          st <- get
          v <- transfer(st, binary, lv, rv)
        } yield v
      case EVariadic(vop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          st <- get
        } yield transfer(st, vop, vs)
      case EMathOp(mop, exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          st <- get
        } yield transfer(st, mop, vs)
      case EConvert(cop, expr) =>
        import COp.*
        for {
          v <- transfer(expr)
          r <- cop match
            case ToStr(Some(radix)) => transfer(radix)
            case ToStr(None)        => pure(AbsValue(MathT(10)))
            case _                  => pure(AbsValue.Bot)
          given AbsState <- get
        } yield v.convertTo(cop, r)
      case EExists(ref) =>
        for {
          v <- get(_.exists(ref))
        } yield v
      case ETypeOf(base) =>
        for {
          v <- transfer(base)
          given AbsState <- get
        } yield v.typeOf
      case EInstanceOf(expr, target) =>
        for {
          v <- transfer(expr)
          t <- transfer(target)
        } yield v.instanceOf(t)
      case ETypeCheck(expr, ty) =>
        for {
          v <- transfer(expr)
          b <- get(_.typeCheck(v, ty.toValue))
        } yield AbsValue(b)
      case ESizeOf(expr) =>
        for {
          v <- transfer(expr)
          given AbsState <- get
        } yield v.sizeOf
      case EClo(fname, captured) => AbsValue(CloT(fname))
      // TODO for {
      //   given AbsState <- get
      //   vs <- join(captured.map(transfer))
      //   pairs = captured zip vs
      //   _ <- join(pairs.map { (x, v) =>
      //     val record = v.ty.record match
      //       case RecordTy.Top => RecordTy.Top
      //       case RecordTy.Elem(map) =>
      //         RecordTy.Elem(map.map { (x, fm) => x -> FieldMap.Top })
      //     val newV = AbsValue(ValueTy(record = record))
      //     modify(_.update(x, newV, refine = true))
      //   })
      // } yield AbsValue(CloT(fname))
      case ECont(fname) => AbsValue(ContT(cfg.fnameMap(fname).id))
      case EDebug(expr) =>
        for {
          v <- transfer(expr)
          st <- get
          _ = debug(s"[[ $expr @ $np ]]($st) = $v")
        } yield v
      case ERandom() => pure(NumberTop)
      case ESyntactic(name, _, rhsIdx, _) =>
        pure(AbsValue(AstT(name, rhsIdx)))
      case ELexical(name, expr) => pure(AbsValue(AstT))
      case ERecord(
            tname @ "CompletionRecord",
            List(
              ("Type", EEnum("normal")),
              ("Value", expr),
              ("Target", EEnum("empty")),
            ),
          ) if inferTypeGuard =>
        for {
          v <- transfer(expr)
          given AbsState <- get
          newV = v.symty match
            case STy(sty) => AbsValue(NormalT(sty))
            case s        => AbsValue(SNormal(s))
        } yield newV
      case ERecord(tname, fields) =>
        for {
          pairs <- join(fields.map {
            case (f, expr) =>
              for {
                v <- transfer(expr)
              } yield (f, v)
          })
          lv <- id(_.allocRecord(tname, pairs))
        } yield lv
      case EMap((kty, vty), _) => AbsValue(MapT(kty.toValue, vty.toValue))
      case EList(exprs) =>
        for {
          vs <- join(exprs.map(transfer))
          lv <- id(_.allocList(vs))
        } yield lv
      case ECopy(obj) =>
        for {
          v <- transfer(obj)
          lv <- id(_.copy(v))
        } yield lv
      case EKeys(map, intSorted) =>
        for {
          v <- transfer(map)
          lv <- id(_.keys(v, intSorted))
        } yield lv
      case EMath(n)              => AbsValue(MathT(n))
      case EInfinity(pos)        => AbsValue(InfinityT(pos))
      case ENumber(n) if n.isNaN => AbsValue(NumberT(Number(Double.NaN)))
      case ENumber(n)            => AbsValue(NumberT(Number(n)))
      case EBigInt(n)            => AbsValue(BigIntT)
      case EStr(str)             => AbsValue(StrT(str))
      case EBool(b)              => AbsValue(BoolT(b))
      case EUndef()              => AbsValue(UndefT)
      case ENull()               => AbsValue(NullT)
      case EEnum(name)           => AbsValue(EnumT(name))
      case ECodeUnit(c)          => AbsValue(CodeUnitT)
    }

    // short circuit evaluation
    def shortCircuit(
      binary: EBinary,
      left: Expr,
      right: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = for {
      l <- transfer(left)
      given AbsState <- get
      v <- binary.bop match {
        case BOp.And =>
          val r: Result[AbsValue] = (st: AbsState) =>
            var bools = Set[Boolean]()
            val lbools = l.ty.bool.set
            if (lbools.contains(false)) bools += false
            if (lbools.contains(true)) {
              val (r, _) = transfer(right)(refine(left, l, TrueT)(st))
              bools ++= r.ty.bool.set
            }
            (AbsValue(BoolT(bools)), st)
          r
        case BOp.Or =>
          val r: Result[AbsValue] = (st: AbsState) =>
            var bools = Set[Boolean]()
            val lbools = l.ty.bool.set
            if (lbools.contains(true)) bools += true
            if (lbools.contains(false))
              val (r, _) = transfer(right)(refine(left, l, FalseT)(st))
              bools ++= r.ty.bool.set
            (AbsValue(BoolT(bools)), st)
          r
        case _ =>
          for {
            r <- transfer(right)
            st <- get
            v = transfer(st, binary, l, r)
          } yield v
      }
    } yield v

    /** construct a symbolic predicate with the current symbolic predicate */
    def lift(constr: TypeConstr = TypeConstr())(using
      st: AbsState,
    ): TypeConstr =
      st.constr && constr

    /** get a type guard */
    def getTypeGuard(expr: Expr)(using np: NodePoint[_]): Result[TypeGuard] = {
      import RefinementKind.*
      expr match {
        case EBool(bool) =>
          val kind = RefinementKind(BoolT(bool))
          for constr <- concretize(lift())
          yield TypeGuard(Map(kind -> constr))
        case ERecord(tname @ "CompletionRecord", fields) =>
          for {
            pairs <- join(fields.map {
              case (f, expr) =>
                for {
                  v <- transfer(expr)
                } yield (f, v)
            })
            v <- id(_.allocRecord(tname, pairs))
            given AbsState <- get
          } yield v.genTypeGuard
        case EBinary(BOp.Lt, l, r) =>
          for {
            lv <- transfer(l)
            rv <- transfer(r)
            given AbsState <- get
          } yield {
            val (lty, rty) = (lv.ty, rv.ty)
            val (lmath, rmath) = (lty.math, rty.math)
            def aux(
              lty: ValueTy,
              rty: ValueTy,
              pos: Boolean,
              isLt: Boolean,
            ): Option[ValueTy] = {
              var math = lty.math
              val infinity = lty.infinity --
                (if (!(isLt ^ pos)) InfinityTy.Pos else InfinityTy.Neg)
              if (lty.math <= MathTy.Int) rty.getSingle match
                case One(Math(0)) =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < 0 */ MathTy.NegInt
                    case (true, false)  => /* x >= 0 */ MathTy.NonNegInt
                    case (false, true)  => /* x > 0 */ MathTy.PosInt
                    case (false, false) => /* x <= 0 */ MathTy.NonPosInt
                case One(Math(v)) if v < 0 =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < N */ MathTy.NegInt
                    case (true, false)  => /* x >= N */ MathTy.Int
                    case (false, true)  => /* x > N */ MathTy.Int
                    case (false, false) => /* x <= N */ MathTy.NegInt
                case One(Math(v)) if v > 0 =>
                  math = (isLt, pos) match
                    case (true, true)   => /* x < P */ MathTy.Int
                    case (true, false)  => /* x >= P */ MathTy.PosInt
                    case (false, true)  => /* x > P */ MathTy.PosInt
                    case (false, false) => /* x <= P */ MathTy.Int
                case _ =>
              val refinedTy = ValueTy(
                math = math,
                infinity = infinity,
                number = lty.number,
                bigInt = lty.bigInt,
              )
              if (lty != refinedTy) Some(refinedTy) else None
            }

            val lguard = TypeGuard((for {
              pos <- List(true, false)
              ref <- toSymRef(l, lv)
              goal <- aux(lty, rty, pos, isLt = true)
              pair <- genConstrPair(ref, lty, goal, BoolT(pos))
            } yield pair).toMap)

            val rguard = TypeGuard((for {
              pos <- List(true, false)
              ref <- toSymRef(r, rv)
              goal <- aux(rty, lty, pos, isLt = false)
              pair <- genConstrPair(ref, rty, goal, BoolT(pos))
            } yield pair).toMap)

            val guard = lguard && rguard
            guard.filter(BoolT).withCur
          }

        case EBinary(BOp.Eq, ERef(ref), r) =>
          for {
            lv <- transfer(ref)
            rv <- transfer(r)
            given AbsState <- get
          } yield {
            val (lty, rty) = (lv.ty, rv.ty)
            val goal = Map(
              TrueT -> (lty && rty),
              FalseT -> (if (rty.isSingle) lty -- rty else lty),
            )
            TypeGuard(for {
              (kind, goalTy) <- goal
              ref <- toSymRef(ref, lv)
              pair <- genConstrPair(ref, lty, goalTy, kind)
            } yield pair)
          }
        case ETypeCheck(ERef(ref), givenTy) =>
          for {
            lv <- transfer(ref)
            given AbsState <- get
          } yield {
            val (lty, rty) = (lv.ty, givenTy.toValue)
            val goal = Map(
              TrueT -> (lty && rty),
              FalseT -> (lty -- rty),
            )
            TypeGuard(for {
              (kind, goalTy) <- goal
              ref <- toSymRef(ref, lv)
              pair <- genConstrPair(ref, lty, goalTy, kind)
            } yield pair)
          }
        case EExists(Field(x: Local, EStr(field))) =>
          val binding = Binding.Exist
          for {
            lv <- transfer(x)
            given AbsState <- get
          } yield {
            val lty = lv.ty
            def aux(binding: Binding) = ValueTy(
              ast = lty.ast,
              record = lty.record.update(field, binding, refine = true),
            )
            val goal = Map(
              TrueT -> aux(binding),
              FalseT -> aux(lty.record(field) -- binding),
            )
            TypeGuard(for {
              (kind, goalTy) <- goal
              ref <- toSymRef(x, lv)
              pair <- genConstrPair(ref, lty, goalTy, kind)
            } yield pair)
          }
        case EExists(Field(x: Local, field)) =>
          for {
            bv <- transfer(x)
            fv <- transfer(field)
            given AbsState <- get
          } yield {
            TypeGuard(
              (for {
                bref <- toSymRef(x, bv)
                fref <- toSymRef(field, fv)
                pexpr = SEExists(SField(bref, SRef(fref)))
              } yield RefinementKind(TrueT) -> TypeConstr(
                sexpr = Some(pexpr, Provenance(np.func)),
              )).toMap,
            )
          }
        case EBinary(BOp.Eq, ETypeOf(l), ETypeOf(r)) =>
          for {
            lv <- transfer(l)
            rv <- transfer(r)
          } yield {
            TypeGuard(
              (for {
                lref <- toSymRef(l, lv)
                rref <- toSymRef(r, rv)
                ltypeOf = SETypeOf(SERef(lref))
                rtypeOf = SETypeOf(SERef(rref))
                pexpr = SEEq(ltypeOf, rtypeOf)
              } yield RefinementKind(TrueT) -> TypeConstr(
                sexpr = Some(pexpr, Provenance(np.func)),
              )).toMap,
            )
          }
        case EBinary(BOp.Eq, ETypeOf(ERef(ref)), r) =>
          for {
            lv <- transfer(ref)
            rv <- transfer(r)
            given AbsState <- get
          } yield {
            val (lty, rty) = (lv.ty, rv.ty)
            def aux(positive: Boolean): ValueTy = rty.str.getSingle match
              case One(tname) =>
                val vty = ValueTy.fromTypeOf(tname)
                if (positive) lty && vty else lty -- vty
              case _ => lty

            val goal = Map(
              TrueT -> aux(true),
              FalseT -> aux(false),
            )
            TypeGuard(for {
              (kind, goalTy) <- goal
              ref <- toSymRef(ref, lv)
              pair <- genConstrPair(ref, lty, goalTy, kind)
            } yield pair)
          }
        case EUnary(UOp.Not, e) =>
          for {
            v <- transfer(e)
            guard = v.guard
            given AbsState <- get
          } yield {
            val (lt, lf) =
              (guard(RefinementKind(TrueT)), guard(RefinementKind(FalseT)))
            TypeGuard(
              RefinementKind(TrueT) -> lift(lf),
              RefinementKind(FalseT) -> lift(lt),
            )
          }
        case EBinary(BOp.Or, l, r) =>
          for {
            lv <- transfer(l)
            lty <- withState(lv.ty)
            rv <- transfer(r)
            rty <- withState(rv.ty)
            lguard = lv.guard
            st <- get
          } yield {
            given AbsState = st
            val hasT = lty.bool.contains(true)
            var guard: Map[RefinementKind, TypeConstr] = Map()
            val (lt, lf) =
              (lguard(RefinementKind(TrueT)), lguard(RefinementKind(FalseT)))
            val refinedSt = if (lf.isTop) st else refine(lf)(st)
            val (thenConstr, _) = (for {
              rv <- transfer(r)
              rt = rv.guard(RefinementKind(TrueT))
            } yield if (hasT) lt || rt else rt)(refinedSt)
            if (thenConstr.nonTop)
              guard = guard + (RefinementKind(TrueT) -> lift(thenConstr))
            val (elseConstr, _) = (for {
              rv <- transfer(r)
              rf = rv.guard(RefinementKind(FalseT))
              hasF = lty.bool.contains(false)
            } yield lf && rf)(refinedSt)
            if (elseConstr.nonTop)
              guard = guard + (RefinementKind(FalseT) -> lift(elseConstr))
            TypeGuard(guard)
          }
        case EBinary(BOp.And, l, r) =>
          for {
            lv <- transfer(l)
            st <- get
            given AbsState = st
            lty = lv.ty
            rv <- transfer(r)
            rty = rv.ty
            hasF = lty.bool.contains(false)
            lguard = lv.guard
          } yield {
            var guard: Map[RefinementKind, TypeConstr] = Map()
            val (lt, lf) =
              (lguard(RefinementKind(TrueT)), lguard(RefinementKind(FalseT)))
            val refinedSt = if (lt.isTop) st else refine(lt)(st)
            val (thenConstr, _) = (for {
              rv <- transfer(r)
              rt = rv.guard(RefinementKind(TrueT))
            } yield lt && rt)(refinedSt)
            if (thenConstr.nonTop)
              guard += RefinementKind(TrueT) -> lift(thenConstr)
            val (elseConstr, _) = (for {
              rv <- transfer(r)
              rf = rv.guard(RefinementKind(FalseT))
            } yield if (hasF) lf || rf else rf)(refinedSt)
            if (elseConstr.nonTop)
              guard += RefinementKind(FalseT) -> lift(elseConstr)
            TypeGuard(guard)
          }
        case EEnum(name) =>
          if RefinementKind.set.contains(EnumT(name)) then
            get(st => {
              TypeGuard(
                Map(
                  RefinementKind(EnumT(name)) -> lift(TypeConstr())(using st),
                ),
              )
            })
          else TypeGuard.Empty
        case _ => TypeGuard.Empty
      }
    }

    /** transfer function for references */
    def transfer(
      ref: Ref,
    )(using np: NodePoint[Node]): Result[AbsValue] = ref match
      // a precise type of `the active function object` in built-in functions
      case Field(
            Field(Global("EXECUTION_STACK"), EMath(0)),
            EStr("Function"),
          ) if np.func.isBuiltin =>
        AbsValue(RecordT("Constructor"))
      // a precise type for intrinsic objects
      case Field(Field(base, EStr("Intrinsics")), EStr(name)) =>
        for {
          b <- transfer(base)
          given AbsState <- get
          v <-
            if (b.ty <= RealmT) {
              val ty = cfg.init.intr.kinds.getOrElse(
                name,
                if (name.startsWith("%Symbol.")) SymbolT else ObjectT,
              )
              pure(AbsValue(ty))
            } else transfer(base)
        } yield v
      case x: Var =>
        for {
          v <- get(_.get(x))
        } yield v
      case field @ Field(base, expr) =>
        for {
          b <- transfer(base)
          p <- transfer(expr)
          given AbsState <- get
          v <- get(_.get(b, p))
        } yield v

    /** transfer function for unary operators */
    def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import UOp.*
      given AbsState = st
      if (config.checkUnaryOp)
        val operandTy = operand.ty
        unary.uop match
          case Abs | Floor =>
            checkUnary(unary, operandTy, MathT)
          case Neg | BNot =>
            checkUnary(unary, operandTy, MathT || NumberT || BigIntT)
          case Not =>
            checkUnary(unary, operandTy, BoolT)
      unary.uop match
        case Neg   => -operand
        case Not   => !operand
        case BNot  => ~operand
        case Abs   => operand.abs
        case Floor => operand.floor
    }

    private def checkUnary(
      unary: EUnary,
      operandTy: ValueTy,
      expectedTys: ValueTy,
    )(using np: NodePoint[Node]): Unit = if (!(operandTy <= expectedTys))
      addError(UnaryOpTypeMismatch(UnaryOpPoint(np, unary), operandTy))

    /** transfer function for binary operators */
    def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue = {
      import BOp.*
      given AbsState = st
      if (config.checkBinaryOp)
        val (lhsTy, rhsTy) = (left.ty, right.ty)
        binary.bop match
          case Add | Sub | Mul | Pow | Div | UMod | Mod | Lt | Equal =>
            checkBinary(binary, lhsTy, rhsTy, Set(ExtMathT, NumberT, BigIntT))
          case LShift | SRShift | URShift | BAnd | BOr | BXOr =>
            checkBinary(binary, lhsTy, rhsTy, Set(MathT, BigIntT))
          case And | Or | Xor =>
            checkBinary(binary, lhsTy, rhsTy, Set(BoolT))
          case Eq =>
      binary.bop match {
        case BAnd    => left & right
        case BOr     => left | right
        case BXOr    => left ^ right
        case Eq      => left =^= right
        case Equal   => left ==^== right
        case Lt      => left < right
        case And     => left && right
        case Or      => left || right
        case Xor     => left ^^ right
        case Add     => left + right
        case Sub     => left sub right
        case Div     => left / right
        case Mul     => left * right
        case Mod     => left % right
        case UMod    => left %% right
        case Pow     => left ** right
        case LShift  => left << right
        case SRShift => left >> right
        case URShift => left >>> right
      }
    }

    private def checkBinary(
      binary: EBinary,
      lhsTy: ValueTy,
      rhsTy: ValueTy,
      expectedTys: Set[ValueTy],
    )(using np: NodePoint[Node]): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        val binaryPoint = BinaryOpPoint(np, binary)
        addError(BinaryOpTypeMismatch(binaryPoint, lhsTy, rhsTy))

    /** transfer for variadic operators */
    def transfer(
      st: AbsState,
      vop: VOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue =
      given AbsState = st
      vop match
        case VOp.Min =>
          val math = vs.map(_.ty.math).reduce(_ min _)
          val inf = vs.map(_.ty.infinity).reduce(_ || _)
          AbsValue(
            ValueTy(
              math = math,
              infinity = if (math.isBottom) inf else inf && InfinityTy.Neg,
            ),
          )

        case VOp.Max =>
          val math = vs.map(_.ty.math).reduce(_ max _)
          val inf = vs.map(_.ty.infinity).reduce(_ || _)
          AbsValue(
            ValueTy(
              math = math,
              infinity = if (math.isBottom) inf else inf && InfinityTy.Pos,
            ),
          )
        case VOp.Concat => StrTop

    /** transfer for mathematical operators */
    def transfer(
      st: AbsState,
      mop: MOp,
      vs: List[AbsValue],
    )(using np: NodePoint[Node]): AbsValue = MathTop

    // =========================================================================
    // Implementation for TyChecker
    // =========================================================================

    // -------------------------------------------------------------------------
    // Instantiation of Symbolic Expressions and References
    // -------------------------------------------------------------------------

    /** Instantiation of an abstract value for calls. If the expression used to
      * call a function can be used as a reference, then instantiate with it
      * instead of the abstract value.
      *
      * @param value
      *   abstract value to instantiate
      * @return
      *   instantiated abstract value
      */
    def instantiateCall(
      value: AbsValue,
      callerNp: NodePoint[Call],
    ): AbsValue =
      import RefinementKind.*
      given callerSt: AbsState = callInfo(callerNp)
      val call = callerNp.node
      val argsInfo = analyzer.argsInfo.getOrElse(callerNp, Nil)
      val map = argsInfo.zipWithIndex.map {
        case ((e, v), i) =>
          i -> (toSymRef(e, v) match
            case Some(ref) if inferTypeGuard => AbsValue(SRef(ref))
            case _                           => v
          )
      }.toMap
      val newV = instantiate(call, value, map)
      if (inferTypeGuard) newV.addGuard(newV.genTypeGuard)
      else newV

    /** instantiation of abstract values */
    def instantiate(
      call: Call,
      value: AbsValue,
      map: Map[Sym, AbsValue],
    )(using st: AbsState): AbsValue =
      val AbsValue(symty, guard) = value
      val newGuard = TypeGuard((for {
        (kind, constr) <- guard.map
        newPred = instantiate(call, constr, map)
        if newPred.nonTop
      } yield kind -> newPred).toMap)
      val ivalue @ AbsValue(isymty, iguard) = instantiate(symty, map)
      AbsValue(isymty, newGuard && iguard)

    /** instantiation of symbolic predicate */
    def instantiate(
      call: Call,
      constr: TypeConstr,
      map: Map[Sym, AbsValue],
    )(using st: AbsState): TypeConstr = TypeConstr(
      map = for {
        case (x: Sym, (ty, prov)) <- constr.map
        v <- map.get(x)
        y <- v.symty match // base to restrict
          case SRef(ref) => Some(ref)
          case _         => None
        (z, zty) <- reduceToBase(y -> ty)
        if !(st.getTy(z) <= zty)
      } yield z -> (zty, prov.forReturn(call)),
      sexpr = for {
        (e, prov) <- constr.sexpr
        newExpr <- instantiate(e, map)
      } yield (newExpr, prov.forReturn(call)),
    )

    /** instantiation of symbolic expressions */
    def instantiate(
      sexpr: SymExpr,
      map: Map[Sym, AbsValue],
    )(using st: AbsState): Option[SymExpr] = sexpr match {
      case SEBool(b) => Some(sexpr)
      case SERef(ref) =>
        instantiate(ref, map).symty match
          case SRef(ref) => Some(SERef(ref))
          case _         => None
      case SEExists(ref) => None
      case SETypeCheck(base, ty) =>
        instantiate(base, map) match
          case Some(e) => Some(SETypeCheck(e, ty))
          case _       => None
      case SETypeOf(base) =>
        instantiate(base, map) match
          case Some(e) => Some(SETypeOf(e))
          case _       => None
      case SEEq(left, right) =>
        val l = instantiate(left, map)
        val r = instantiate(right, map)
        (l, r) match
          case (Some(l), Some(r)) => Some(SEEq(l, r))
          case _                  => None
      case SEOr(left, right)  => ???
      case SEAnd(left, right) => ???
      case SENot(expr)        => ???
    }

    /** instantiation of symbolic references */
    def instantiate(
      sref: SymRef,
      map: Map[Sym, AbsValue],
    )(using st: AbsState): AbsValue = sref match {
      case SBase(x: Sym) => map.getOrElse(x, AbsValue.Bot)
      case SField(b, f)  => st.get(instantiate(b, map), instantiate(f, map))
      case _             => AbsValue.Bot
    }

    /** instantiation of symbolic type */
    def instantiate(
      symty: SymTy,
      map: Map[Sym, AbsValue],
    )(using st: AbsState): AbsValue = symty match
      case STy(ty)   => AbsValue(ty)
      case SRef(ref) => instantiate(ref, map)
      case SNormal(symty) =>
        val ty = instantiate(symty, map).symty match
          case STy(ty) => STy(NormalT(ty))
          case s       => SNormal(s)
        AbsValue(ty)

    /** Syntactic refinement, which is used before employing the type guard.
      * This is deprecated and only used for experimental purposes.
      *
      * @param cond
      *   a condition expression to refine
      * @param positive
      *   a flag to indicate the polarity of the condition
      * @param np
      *   the node point used to create provenance
      * @return
      *   the updater to refine the state
      */
    def syntacticRefine(
      cond: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = cond match {
      // refine inequality
      case EBinary(BOp.Lt, l, r) =>
        def toLocal(e: Expr): Option[Local] = e match
          case ERef(x: Local) => Some(x)
          case _              => None
        for {
          lv <- transfer(l)
          rv <- transfer(r)
          given AbsState <- get
          lmath = lv.ty.math
          rmath = rv.ty.math
          _ <- modify { st =>
            val lst = toLocal(l).fold(st) { x =>
              var math = lmath
              var infinity = lv.ty.infinity --
                (if (positive) InfinityTy.Pos else InfinityTy.Neg)
              val refined = (r, rmath) match
                case (EMath(0), _) =>
                  math = if (positive) MathTy.NegInt else MathTy.NonNegInt
                case l =>
              st.update(
                x,
                AbsValue(
                  ValueTy(
                    math = math,
                    infinity = infinity,
                    number = lv.ty.number,
                    bigInt = lv.ty.bigInt,
                  ),
                ),
                refine = true,
              )
            }
            toLocal(r).fold(lst) { x =>
              var math = rmath
              var infinity = rv.ty.infinity --
                (if (positive) InfinityTy.Neg else InfinityTy.Pos)
              val refined = (l, lmath) match
                case (EMath(0), _) =>
                  math = if (positive) MathTy.PosInt else MathTy.NonPosInt
                case _ => rmath
              lst.update(
                x,
                AbsValue(
                  ValueTy(
                    math = math,
                    infinity = infinity,
                    number = rv.ty.number,
                    bigInt = rv.ty.bigInt,
                  ),
                ),
                refine = true,
              )
            }
          }
        } yield ()
      // refine local variables
      case EBinary(BOp.Eq, ERef(x: Local), expr) =>
        refineLocal(x, expr, positive)
      // refine field equality
      case EBinary(BOp.Eq, ERef(Field(x: Local, EStr(field))), expr) =>
        refineField(x, field, expr, positive)
      // refine field existence
      case EExists(Field(x: Local, EStr(field))) =>
        refineExistField(x, field, positive)
      // refine types
      case EBinary(BOp.Eq, ETypeOf(ERef(x: Local)), expr) =>
        refineType(x, expr, positive)
      // refine type checks
      case ETypeCheck(ERef(ref), ty) =>
        refineTypeCheck(ref, ty.ty.toValue, positive)
      // refine logical negation
      case EUnary(UOp.Not, e) =>
        syntacticRefine(e, !positive)
      // refine logical disjunction
      case EBinary(BOp.Or, l, r) =>
        st =>
          if (positive)
            syntacticRefine(l, true)(st) ⊔ syntacticRefine(r, true)(st)
          else syntacticRefine(r, false)(syntacticRefine(l, false)(st))
      // refine logical conjunction
      case EBinary(BOp.And, l, r) =>
        st =>
          if (positive) syntacticRefine(r, true)(syntacticRefine(l, true)(st))
          else syntacticRefine(l, false)(st) ⊔ syntacticRefine(r, false)(st)
      // no pruning
      case _ => st => st
    }

    /** Returns a updater which refines a state according to the given type
      * constraint.
      *
      * @param constr
      *   a type constraint
      * @param np
      *   the node point used to create provenance
      * @return
      *   the updater to refine the state
      */
    def refine(
      constr: TypeConstr,
    )(using np: NodePoint[_]): Updater = {

      // handles "typeof" operator of ECMAScript
      def typeOfType(givenTy: ValueTy): ValueTy = {
        var ty = BotT
        givenTy.typeOfNames.map {
          case "Number"    => ty ||= NumberT
          case "BigInt"    => ty ||= BigIntT
          case "String"    => ty ||= StrT
          case "Boolean"   => ty ||= BoolT
          case "Undefined" => ty ||= UndefT
          case "Null"      => ty ||= NullT
          case "Object"    => ty ||= ObjectT
          case "Symbol"    => ty ||= SymbolT
          case _           =>
        }
        ty
      }

      val TypeConstr(map, expr, bot) = constr
      if bot then return pure(()) // should not occur
      val alias: Map[SymBase, SymBase] = expr.fold(Map()) {
        case (SEEq(SETypeOf(SERef(SBase(x))), SETypeOf(SERef(SBase(y)))), _) =>
          Map(x -> y, y -> x)
        case _ => Map()
      }
      for {
        _ <- join(map.map {
          case (x, (ty, prov)) =>
            for {
              _ <- modify(refine(x, ty))
              _ <- alias.get(x) match
                case Some(y) => modify(refine(y, typeOfType(ty)))
                case None    => pure(())
            } yield ()
        })
        // _ <- expr.fold(pure(()))((e, prov) => modify(refine(e)))
        _ <- modify(st => st.copy(constr = lift(constr)(using st)))
      } yield ()
    }

    /** When the `base` should be refined to the type `ty`, then this method
      * returns the updater to refine the type of the base.
      *
      * @param base
      *   the base to be refined
      * @param ty
      *   the type to be refined
      * @param np
      *   the node point used to create provenance
      * @return
      *   the updater to refine the type of the base
      */
    def refine(
      base: SymBase,
      ty: ValueTy,
    )(using np: NodePoint[_]): Updater = base match
      case sym: Sym =>
        st =>
          val refinedTy = st.symEnv.get(sym).fold(ty)(_ && ty)
          st.copy(symEnv = st.symEnv + (sym -> refinedTy))
      case x: Local =>
        for {
          v <- transfer(x)
          given AbsState <- get
          refinedV = v ⊓ AbsValue(ty)
          _ <- modify(_.update(x, refinedV, refine = true))
          _ <- v.noticeRefinedTo(refinedV)
        } yield ()

    /** When the `ref` should be refined to the type `ty`, then this method
      * returns the type base of the reference should be refined. The returned
      * type base is a pair of the reference and the type, plus provenance.
      *
      * @param ref
      *   the reference to be refined
      * @param ty
      *   the type to be refined
      * @param np
      *   the node point used to create provenance
      * @return
      *   the type base of the reference should be refined
      */
    def reduceToBase(
      ref: SymRef,
      ty: ValueTy,
      np: NodePoint[_],
    )(using
      st: AbsState,
    ): Option[(SymBase, (ValueTy, Provenance))] =
      reduceToBase(ref -> ty)(using st).map { (base, ty) =>
        base -> (ty, Provenance(np.func))
      }

    def reduceToBase(pair: (SymRef, ValueTy), np: NodePoint[_])(using
      st: AbsState,
    ): Option[(SymBase, (ValueTy, Provenance))] =
      reduceToBase(pair._1, pair._2, np)

    /** When the `ref` should be refined to the type `ty`, then this method
      * returns the type base of the reference should be refined. The returned
      * type base is a pair of the reference and the type.
      *
      * @param ref
      *   the reference to be refined
      * @param givenTy
      *   the type to be refined
      * @return
      *   the type base of the reference should be refined
      */
    def reduceToBase(
      ref: SymRef,
      givenTy: ValueTy,
    )(using st: AbsState): Option[(SymBase, ValueTy)] = {
      ref match
        case SBase(x) =>
          val ty = st.getTy(x)
          if (ty <= givenTy) None else Some(x -> givenTy)
        case SField(base, STy(x)) if x <= StrT && x.isSingle =>
          val bty = st.getTy(base)
          val field = x.str.getSingle match
            case One(elem) => elem
            case _         => return None
          val refinedTy = ValueTy(
            ast = bty.ast,
            record = bty.record.update(field, givenTy, refine = true),
          )
          reduceToBase(base -> refinedTy)
        case _ => None
    }
    def reduceToBase(pair: (SymRef, ValueTy))(using
      st: AbsState,
    ): Option[(SymBase, ValueTy)] = reduceToBase(pair._1, pair._2)

    /** Refine the local variable `x` when the `x` is ensured to be
      * equal/inequal to the evaluation of the `expr`.
      *
      * @param x
      *   a local variable to be refined
      * @param expr
      *   an expression
      * @param positive
      *   `x` and `expr` are equal if `positive` is true, otherwise inequal
      * @param np
      *   the node point used to create provenance
      * @return
      *   Updater to refine the local variable `x`
      */
    def refineLocal(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      rv <- transfer(expr)
      refinedV <- withState {
        if (positive) lv ⊓ rv
        else if (rv.isSingle) lv -- rv
        else lv
      }
      _ <- modify(_.update(x, refinedV, refine = true))
      _ <- lv.noticeRefinedTo(refinedV)
    } yield ()

    /** TODO refine types with field equality */
    def refineField(
      x: Local,
      field: String,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      binding <- withState { Binding(rv.ty) }
      _ <- refineField(x, field, binding, positive)
    } yield ()

    def refineField(
      x: Local,
      field: String,
      rbinding: Binding,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      lty <- withState(lv.ty)
      binding = if (positive) rbinding else lty.record(field) -- rbinding
      refinedTy = ValueTy(
        ast = lty.ast,
        record = lty.record.update(field, binding, refine = true),
      )
      refinedV = AbsValue(refinedTy)
      _ <- modify(_.update(x, refinedV, refine = true))
      _ <- lv.noticeRefinedTo(refinedV)
    } yield ()

    /** refine types with field existence */
    def refineExistField(
      x: Local,
      field: String,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      refineField(x, field, Binding.Exist, positive)

    /** refine types with `typeof` constraints */
    def refineType(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      lv <- transfer(x)
      rv <- transfer(expr)
      lty <- withState(lv.ty)
      rty <- withState(rv.ty)
      refinedV <- withState {
        rty.str.getSingle match
          case One(tname) =>
            val value = AbsValue(ValueTy.fromTypeOf(tname))
            if (positive) lv ⊓ value else lv -- value
          case _ => lv
      }
      _ <- modify(_.update(x, refinedV, refine = true))
    } yield ()

    /** refine types with type checks */
    def refineTypeCheck(
      ref: Ref,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      v <- transfer(ref)
      given AbsState <- get
      refinedV =
        if (positive) v ⊓ AbsValue(ty)
        else v -- AbsValue(ty)
      _ <- modify(ref match
        case x: Local => _.update(x, refinedV, refine = true)
        case Field(x: Local, EStr(field)) =>
          refineField(x, field, Binding(ty), positive)
        case _ => identity,
      )
      _ <- v.noticeRefinedTo(refinedV)
    } yield ()

    /** check if the return type can be used */
    private lazy val canUseReturnTy: Func => Boolean = cached { func =>
      manualRefiners.contains(func.name) || (
        !func.retTy.isImprec &&
        RefinementKind.from(func.retTy.ty.toValue).isEmpty
      )
    }

    extension (value: AbsValue) {

      /** Call this to propagate refinements when the abstract value is refined.
        *
        * @param refinedValue
        *   the refined abstract value
        * @return
        *   an updater that propagates the refinements
        */
      def noticeRefinedTo(refinedValue: AbsValue)(using
        np: NodePoint[_],
      ): Updater =
        import RefinementKind.*
        given AbsState = getState(np)
        val refined = refinedValue.ty
        join(for {
          (kind, constr) <- value.guard.map
          if refined <= kind.ty
        } yield refine(constr))
    }

    /** Generate a pair of a refinement kind and a type constraint(if it is
      * possible).
      *
      * @param ref
      *   the reference to be refined
      * @param ty
      *   the current type of the reference
      * @param goalTy
      *   the goal type of the reference
      * @param kind
      *   refinement kind
      * @return
      *   a pair of a refinement kind and a type constraint
      */
    def genConstrPair(ref: SymRef, ty: ValueTy, goalTy: ValueTy, kind: ValueTy)(
      using
      st: AbsState,
      np: NodePoint[_],
    ): Option[(RefinementKind, TypeConstr)] =
      if (ty != goalTy && !goalTy.isBottom)
        reduceToBase(ref -> goalTy, np).flatMap { pair =>
          Some(RefinementKind(kind) -> lift(TypeConstr(pair)))
        }
      else None

    /** default type guards */
    type Refinements = Map[RefinementKind, Map[Local, ValueTy]]
    type Refinement = (Func, List[AbsValue], ValueTy, AbsState) => AbsValue
    val manualRefiners: Map[String, Refinement] = {
      import RefinementKind.*, SymExpr.*, SymRef.*, SymTy.*
      Map(
        "__APPEND_LIST__" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(vs(0).ty || vs(1).ty)
        },
        "__FLAT_LIST__" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(vs(0).ty.list.elem)
        },
        "__GET_ITEMS__" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val ast = vs(1).ty.toValue.grammarSymbol match
            case Fin(set) => AstT(set.map(_.name))
            case Inf      => AstT
          AbsValue(ListT(ast))
        },
        "__CLAMP__" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val refined =
            if (vs(0).ty.toValue <= (IntT || InfinityT))
              if (vs(1).ty.toValue <= MathT(0)) NonNegIntT
              else IntT
            else retTy
          AbsValue(refined)
        },
        "Completion" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(SRef(SBase(0)))
        },
        "NormalCompletion" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(SNormal(SRef(SBase(0))))
        },
        "UpdateEmpty" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val record = vs(0).ty.record
          val valueField = record("Value").value
          val updated = record.update(
            "Value",
            vs(1).ty || (valueField -- EnumT("empty")),
            refine = false,
          )
          AbsValue(ValueTy(record = updated))
        },
        "IteratorClose" -> { (func, vs, retTy, st) =>
          given AbsState = st
          // Throw | #1
          AbsValue(vs(1).ty || ThrowT)
        },
        "AsyncIteratorClose" -> { (func, vs, retTy, st) =>
          given AbsState = st
          // Throw | #1
          AbsValue(vs(1).ty || ThrowT)
        },
        "Await" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(NormalT(ESValueT) || ThrowT)
        },
        "RequireInternalSlot" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val refined = vs(1).ty.str.getSingle match
            case One(f) =>
              ValueTy(
                record = ObjectT.record.update(f, Binding.Exist, refine = true),
              )
            case _ => ObjectT
          val prov = Provenance(func)
          val guard =
            TypeGuard(
              RefinementKind(NormalT) -> TypeConstr(0 -> (refined, prov)),
            )
          AbsValue(STy(retTy), guard)
        },
        "NewPromiseCapability" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val prov = Provenance(func)
          val guard = TypeGuard(
            RefinementKind(NormalT) -> TypeConstr(0 -> (ConstructorT, prov)),
          )
          AbsValue(STy(retTy), guard)
        },
        "CreateListFromArrayLike" -> { (func, vs, retTy, st) =>
          given AbsState = st
          AbsValue(
            (for {
              v <- vs.lift(1)
              str = v.ty.list.elem.str
              ss <- str match
                case Inf     => None
                case Fin(ss) => Some(ss)
              ty = ss.map(ValueTy.fromTypeOf).foldLeft(BotT)(_ || _)
              refined = retTy.toValue && NormalT(ListT(ty))
            } yield refined).getOrElse(retTy),
          )
        },
        "SameType" -> { (func, vs, retTy, st) =>
          given AbsState = st
          val expr = SEEq(SETypeOf(SERef(SBase(0))), SETypeOf(SERef(SBase(1))))
          val prov = Provenance(func)
          AbsValue(
            STy(BoolT),
            TypeGuard(RefinementKind(TrueT) -> TypeConstr(expr -> prov)),
          )
        },
        "TypedArrayElementType" -> { (func, vs, retTy, st) =>
          AbsValue(
            EnumT(
              "int8",
              "uint8",
              "uint8clamped",
              "int16",
              "uint16",
              "int32",
              "uint32",
              "bigint64",
              "biguint64",
              "float32",
              "float64",
            ),
          )
        },
        "TypedArrayElementSize" -> { (func, vs, retTy, st) =>
          AbsValue(PosIntT)
        },
      )
    }
  }
}
