package esmeta.analyzer

import esmeta.{ANALYZE_LOG_DIR, LINE_SEP}
import esmeta.analyzer.domain.*
import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ty.*
import esmeta.ty.util.{Stringifier => TyStringifier}
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.state.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** specification type analyzer for ECMA-262 */
class TypeAnalyzer(
  val cfg: CFG,
  val targetPattern: Option[String] = None,
  val typeSens: Boolean = false, // TODO
  val useTypeGuard: Boolean = true,
  val config: TypeAnalyzer.Config = TypeAnalyzer.Config(),
  val ignore: TypeAnalyzer.Ignore = Ignore(),
  val log: Boolean = false,
  val detail: Boolean = false,
  val silent: Boolean = false,
  override val useRepl: Boolean = false,
  override val replContinue: Boolean = false,
) extends Analyzer { analyzer =>
  import TypeAnalyzer.*

  /** initialization of ECMAScript environment */
  lazy val init: Initialize = new Initialize(cfg)

  /** perform type analysis */
  lazy val analyze: Unit =
    AbsState.setBase(init)
    transfer.fixpoint
    if (log) logging

  /** unused ignore set */
  protected var _unusedSet: Set[String] = ignore.names
  inline def unusedSet: Set[String] = _unusedSet

  /** perform type analysis with the given control flow graph */
  def errors: Set[TypeError] = errorMap.values.toSet
  def detected = errors.filter(error => {
    val name = error.func.name
    _unusedSet -= name
    !ignore.names.contains(name)
  })

  /** all possible initial analysis target functions */
  def targetFuncs: List[Func] =
    val allFuncs = cfg.funcs.filter(f => f.isParamTysPrecise && !f.isCont)
    val funcs = targetPattern.fold(allFuncs)(pattern => {
      val funcs = allFuncs.filter(f => pattern.r.matches(f.name))
      if (!silent && funcs.isEmpty)
        warn(s"failed to find functions matched with the pattern `$pattern`.")
      funcs
    })
    if (!silent) println(s"- ${funcs.size} functions are initial targets.")
    funcs

  /** check if the ignore set needs to be updated */
  def needUpdate: Boolean = detected.nonEmpty || unusedSet.nonEmpty

  /** update ignorance system */
  def updateIgnore: Unit = for (path <- ignore.filename)
    dumpJson(
      name = "algorithm names for the ignorance system",
      data = errors.map(_.func.name).toList.sorted,
      filename = path,
      noSpace = false,
      silent = silent,
    )

  /** no sensitivity */
  override val irSens: Boolean = false

  /** type semantics as results */
  lazy val sem: Semantics = new Semantics
  class Semantics extends AbsSemantics(getInitNpMap(targetFuncs)) {

    /** type analysis result string */
    def typesString: String =
      given getRule: Rule[Iterable[Func]] = (app, funcs) =>
        import TyStringifier.given
        given Rule[Iterable[(String, ValueTy)]] = iterableRule("(", ", ", ")")
        app >> "-" * 80
        for (func <- funcs) {
          val rp = ReturnPoint(func, View())
          app :> "   " >> func.headString
          val fname = func.name
          val entryNp = NodePoint(func, func.entry, View())
          val st = this(entryNp)
          val newParams =
            for (p <- func.params) yield p.lhs.name -> st.get(p.lhs).ty
          app :> "-> " >> "def "
          app >> func.irFunc.kind.toString >> fname >> newParams
          app >> ": " >> rpMap.get(rp).fold(func.retTy.ty)(_.value.ty)
          app :> "-" * 80
        }
        app
      given paramRule: Rule[(String, ValueTy)] = (app, pair) =>
        import TyStringifier.given
        val (param, ty) = pair
        app >> param >> ": " >> ty
      (new Appender >> cfg.funcs.toList.sortBy(_.name)).toString
  }

  /** transfer function */
  lazy val transfer: Transfer = new Transfer
  class Transfer extends AbsTransfer {

    /** loading monads */
    import AbsState.monad.*

    /** assign argument to parameter */
    override def assignArg(
      callPoint: CallPoint,
      method: Boolean,
      idx: Int,
      param: Param,
      arg: AbsValue,
    ): AbsValue =
      val paramTy = param.ty.ty.toValue
      val argTy = arg.ty
      if (method && idx == 0) () /* ignore `this` for method calls */
      else if (config.checkParamType && !(argTy <= paramTy))
        addError(ParamTypeMismatch(ArgAssignPoint(callPoint, idx), argTy))
      AbsValue(paramTy && argTy)

    /** callee entries */
    override def getCalleeEntries(
      callerNp: NodePoint[Call],
      locals: List[(Local, AbsValue)],
    ): List[(View, List[(Local, AbsValue)])] = List(View() -> (for {
      (local, value) <- locals
    } yield local -> AbsValue(value.ty)))

    /** get local variables */
    override def getLocals(
      callPoint: CallPoint,
      method: Boolean,
      vs: List[AbsValue],
    ): List[(Local, AbsValue)] =
      val CallPoint(callerNp, callee) = callPoint
      val arity @ (from, to) = callee.arity
      val len = vs.length
      if (config.checkArity && (len < from || to < len))
        addError(ArityMismatch(callPoint, len))
      super.getLocals(callPoint, method, vs)

    /** get callee state */
    override def getCalleeState(
      callerSt: AbsState,
      locals: List[(Local, AbsValue)],
    ): AbsState = analyzer.getCalleeState(callerSt, locals)

    /** check if the return type can be used */
    private lazy val canUseReturnTy: Func => Boolean = cached { func =>
      !func.retTy.isImprec ||
      (useTypeGuard && typeGuards.contains(func.name)) ||
      defaultTypeGuards.contains(func.name)
    }

    /** handle calls */
    override def doCall(
      callPoint: CallPoint,
      callerSt: AbsState,
      args: List[Expr],
      vs: List[AbsValue],
      captured: Map[Name, AbsValue] = Map(),
      method: Boolean = false,
      tgt: Option[NodePoint[Node]] = None,
    ): Unit =
      val CallPoint(callerNp, callee) = callPoint
      if (canUseReturnTy(callee)) {
        val call = callerNp.node
        val retTy = callee.retTy.ty
        val map = args.zipWithIndex.collect {
          case (ERef(x: Local), i) => i -> SymRef.SLocal(x)
        }.toMap
        val newRetV = (for {
          refine <- typeGuards.get(callee.name)
          if useTypeGuard || defaultTypeGuards.contains(callee.name)
          v = refine(vs, retTy)
          guard = for {
            (kind, pred) <- v.guard
            newPred <- instantiate(pred, map)
          } yield kind -> newPred
          newV = AbsValue(v.ty, Zero, guard)
        } yield newV).getOrElse(AbsValue(retTy))
        for {
          nextNp <- getAfterCallNp(callerNp)
          newSt = callerSt.define(call.lhs, newRetV)
        } sem += nextNp -> newSt
      }
      super.doCall(callPoint, callerSt, args, vs, captured, method, tgt)

    /** instantiation of symbolic expressions */
    def instantiate(sexpr: SymExpr, map: Map[Sym, SymRef]): Option[SymExpr] =
      import SymExpr.*
      sexpr match
        case SEBool(b) => Some(SEBool(b))
        case SEStr(s)  => Some(SEStr(s))
        case SERef(ref) =>
          for { r <- instantiate(ref, map) } yield SERef(r)
        case SETypeCheck(base, ty) =>
          for { b <- instantiate(base, map) } yield SETypeCheck(b, ty)
        case SEBinary(bop, left, right) =>
          for {
            l <- instantiate(left, map)
            r <- instantiate(right, map)
          } yield SEBinary(bop, l, r)
        case SEUnary(uop, expr) =>
          for { e <- instantiate(expr, map) } yield SEUnary(uop, e)

    def instantiate(sref: SymRef, map: Map[Sym, SymRef]): Option[SymRef] =
      import SymRef.*
      sref match
        case SSym(sym) => map.get(sym)
        case SLocal(x) => Some(SLocal(x))
        case SField(base, field) =>
          for {
            b <- instantiate(base, map)
            f <- instantiate(field, map)
          } yield SField(b, f)

    /** propagate callee analysis result */
    override def propagate(rp: ReturnPoint, callerNp: NodePoint[Call]): Unit =
      if (!canUseReturnTy(rp.func))
        val AbsRet(value, st) = sem(rp)
        (for {
          nextNp <- getAfterCallNp(callerNp)
          if !value.isBottom
          callerSt = sem.callInfo(callerNp)
        } yield sem += nextNp -> st.doReturn(
          callerSt,
          callerNp.node.lhs,
          value,
        )).getOrElse(super.propagate(rp, callerNp))

    /** transfer function for return points */
    override def apply(rp: ReturnPoint): Unit =
      if (!canUseReturnTy(rp.func)) super.apply(rp)

    /** default type guards */
    val defaultTypeGuards: Set[String] = Set(
      "__APPEND_LIST__",
      "__FLAT_LIST__",
      "__GET_ITEMS__",
      "__CLAMP__",
      "Completion",
      "NormalCompletion",
      "UpdateEmpty",
    )

    /** type guards */
    type Refinements = Map[RefinementKind, Map[Local, ValueTy]]
    type Refinement = (List[AbsValue], Ty) => AbsValue
    val typeGuards: Map[String, Refinement] =
      import RefinementKind.*, SymExpr.*, SymRef.*
      Map(
        "__APPEND_LIST__" -> { (vs, retTy) =>
          AbsValue(vs(0).ty || vs(1).ty, Zero, Map())
        },
        "__FLAT_LIST__" -> { (vs, retTy) =>
          AbsValue(vs(0).ty.list.elem, Zero, Map())
        },
        "__GET_ITEMS__" -> { (vs, retTy) =>
          val ast = vs(1).ty.toValue.grammarSymbol match
            case Fin(set) => AstT(set.map(_.name))
            case Inf      => AstT
          AbsValue(ListT(ast), Zero, Map())
        },
        "__CLAMP__" -> { (vs, retTy) =>
          val refined =
            if (vs(0).ty.toValue <= (IntT || InfinityT))
              if (vs(1).ty.toValue <= MathT(0)) NonNegIntT
              else IntT
            else retTy
          AbsValue(refined, Zero, Map())
        },
        "Completion" -> { (vs, retTy) =>
          vs(0) ⊓ AbsValue(CompT, Zero, Map())
        },
        "NormalCompletion" -> { (vs, retTy) =>
          AbsValue(NormalT(vs(0).ty -- CompT), Zero, Map())
        },
        "IteratorClose" -> { (vs, retTy) =>
          AbsValue(vs(1).ty || ThrowT, Zero, Map())
        },
        "AsyncIteratorClose" -> { (vs, retTy) =>
          AbsValue(vs(1).ty || ThrowT, Zero, Map())
        },
        "OrdinaryObjectCreate" -> { (vs, retTy) =>
          AbsValue(RecordT("Object"), Zero, Map())
        },
        "UpdateEmpty" -> { (vs, retTy) =>
          val record = vs(0).ty.record
          val valueField = record("Value").value
          val updated = record.update(
            "Value",
            vs(1).ty || (valueField -- EnumT("empty")),
            refine = false,
          )
          AbsValue(ValueTy(record = updated), Zero, Map())
        },
        "MakeBasicObject" -> { (vs, retTy) =>
          AbsValue(RecordT("Object"), Zero, Map())
        },
        "Await" -> { (vs, retTy) =>
          AbsValue(NormalT(ESValueT) || ThrowT, Zero, Map())
        },
        "IsCallable" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), FunctionT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsConstructor" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), ConstructorT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "RequireInternalSlot" -> { (vs, retTy) =>
          val refined = vs(1).ty.str.getSingle match
            case One(f) =>
              ValueTy(
                record = ObjectT.record.update(f, Binding.Exist, refine = true),
              )
            case _ => ObjectT
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), refined),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateTypedArray" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), TypedArrayT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateIntegerTypedArray" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), TypedArrayT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateAtomicAccessOnIntegerTypedArray" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), TypedArrayT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "ValidateNonRevokedProxy" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(
              SERef(SSym(0)),
              ValueTy.from(
                "Record[ProxyExoticObject { ProxyHandler : Record[Object], ProxyTarget : Record[Object] }]",
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPromise" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), RecordT("Promise")),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsRegExp" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            NormalTrue -> SETypeCheck(SERef(SSym(0)), ObjectT),
            Abrupt -> SETypeCheck(SERef(SSym(0)), ObjectT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "NewPromiseCapability" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), ConstructorT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "CreateListFromArrayLike" -> { (vs, retTy) =>
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
            Zero,
            Map(),
          )
        },
        "IsUnresolvableReference" -> { (vs, retTy) =>
          var guard: TypeGuard = Map(
            True -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map("Base" -> EnumT("unresolvable")),
              ),
            ),
            False -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map("Base" -> (ESValueT || RecordT("EnvironmentRecord"))),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPropertyReference" -> { (vs, retTy) =>
          var guard: TypeGuard = Map(
            True -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map("Base" -> ESValueT),
              ),
            ),
            False -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map(
                  "Base" ->
                  (RecordT("EnvironmentRecord") || EnumT("unresolvable")),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsSuperReference" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), RecordT("SuperReferenceRecord")),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsPrivateReference" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map("ReferencedName" -> RecordT("PrivateName")),
              ),
            ),
            False -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "ReferenceRecord",
                Map(
                  "ReferencedName" -> (SymbolT || StrT /* TODO ESValue in latest version */ ),
                ),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsArray" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            NormalTrue -> SETypeCheck(SERef(SSym(0)), ObjectT),
            Abrupt -> SETypeCheck(SERef(SSym(0)), ObjectT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsSharedArrayBuffer" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(
              SERef(SSym(0)),
              RecordT(
                "SharedArrayBuffer",
                Map("ArrayBufferData" -> RecordT("SharedDataBlock")),
              ),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsConcatSpreadable" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            NormalTrue -> SETypeCheck(SERef(SSym(0)), ObjectT),
            Abrupt -> SETypeCheck(SERef(SSym(0)), ObjectT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "IsDetachedBuffer" -> { (vs, retTy) =>
          def getTy(ty: ValueTy, sty: ValueTy) = RecordT(
            Map(
              "ArrayBuffer" -> FieldMap("ArrayBufferData" -> Binding(ty)),
              "SharedArrayBuffer" -> FieldMap("ArrayBufferData" -> Binding(sty)),
            ),
          )
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), getTy(NullT, NullT)),
            False -> SETypeCheck(
              SERef(SSym(0)),
              getTy(RecordT("DataBlock"), RecordT("SharedDataBlock")),
            ),
          )
          AbsValue(retTy, Zero, guard)
        },
        "AllocateArrayBuffer" -> { (vs, retTy) =>
          AbsValue(
            NormalT(
              RecordT(
                "ArrayBuffer",
                FieldMap("ArrayBufferData" -> Binding(RecordT("DataBlock"))),
              ),
            ) || ThrowT,
            Zero,
            Map(),
          )
        },
        "AllocateSharedArrayBuffer" -> { (vs, retTy) =>
          AbsValue(
            NormalT(
              RecordT(
                "SharedArrayBuffer",
                FieldMap(
                  "ArrayBufferData" -> Binding(RecordT("SharedDataBlock")),
                ),
              ),
            ) || ThrowT,
            Zero,
            Map(),
          )
        },
        "CanBeHeldWeakly" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            True -> SETypeCheck(SERef(SSym(0)), ObjectT || SymbolT),
          )
          AbsValue(retTy, Zero, guard)
        },
        "AsyncGeneratorValidate" -> { (vs, retTy) =>
          val guard: TypeGuard = Map(
            Normal -> SETypeCheck(SERef(SSym(0)), RecordT("AsyncGenerator")),
          )
          AbsValue(retTy, Zero, guard)
        },
      )

    /** update return points */
    override def doReturn(
      irp: InternalReturnPoint,
      newRet: AbsRet,
    ): Unit =
      val InternalReturnPoint(NodePoint(func, _, view), irReturn) = irp
      val givenTy = newRet.value.ty
      val expected = func.retTy.ty match
        case _: UnknownTy        => newRet
        case expectedTy: ValueTy =>
          // return type check when it is a known type
          if (config.checkReturnType && !(givenTy <= expectedTy))
            addError(ReturnTypeMismatch(irp, givenTy))
          AbsRet(AbsValue(givenTy && expectedTy))
      super.doReturn(irp, expected)

    /** transfer function for normal instructions */
    override def transfer(inst: NormalInst)(using np: NodePoint[_]): Updater =
      inst match
        case IAssign(Field(x: Var, EStr(f)), expr) =>
          for {
            v <- transfer(expr)
            ty <- get(_.get(x).ty)
            record = ty.record.update(f, v.ty, refine = false)
            _ <- modify(_.update(x, AbsValue(ty.copied(record = record))))
          } yield ()
        case _ => super.transfer(inst)

    /** transfer function for expressions */
    override def transfer(
      expr: Expr,
    )(using np: NodePoint[Node]): Result[AbsValue] = expr match
      // a precise type of `the active function object` in built-in functions
      case ERef(
            Field(
              Field(Global("EXECUTION_STACK"), EMath(0)),
              EStr("Function"),
            ),
          ) if np.func.isBuiltin =>
        AbsValue(RecordT("Constructor"))
      // a precise type for intrinsic objects
      case ERef(
            Field(
              Field(
                Field(
                  Field(Global("EXECUTION_STACK"), EMath(0)),
                  EStr("Realm"),
                ),
                EStr("Intrinsics"),
              ),
              EStr(name),
            ),
          ) =>
        AbsValue(cfg.init.intr.kinds.getOrElse(name, ObjectT))
      case EMap((kty, vty), _) => AbsValue(MapT(kty.toValue, vty.toValue))
      case _                   => super.transfer(expr)

    /** transfer function for unary operators */
    override def transfer(
      st: AbsState,
      unary: EUnary,
      operand: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import UOp.*
      if (config.checkUnaryOp)
        val operandTy = operand.ty
        unary.uop match
          case Abs | Floor =>
            checkUnary(unary, operandTy, MathT)
          case Neg | BNot =>
            checkUnary(unary, operandTy, MathT || NumberT || BigIntT)
          case Not =>
            checkUnary(unary, operandTy, BoolT)
      super.transfer(st, unary, operand)

    private def checkUnary(
      unary: EUnary,
      operandTy: ValueTy,
      expectedTys: ValueTy,
    )(using np: NodePoint[Node]): Unit = if (!(operandTy <= expectedTys))
      addError(UnaryOpTypeMismatch(UnaryOpPoint(np, unary), operandTy))

    /** transfer function for binary operators */
    override def transfer(
      st: AbsState,
      binary: EBinary,
      left: AbsValue,
      right: AbsValue,
    )(using np: NodePoint[Node]): AbsValue =
      import BOp.*
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
      super.transfer(st, binary, left, right)

    /** transfer function for symbolic expressions */
    def transfer(
      expr: SymExpr,
    )(using np: NodePoint[Node]): Result[ValueTy] = ???

    /** transfer function for symbolic references */
    def transfer(
      ref: SymRef,
    )(using np: NodePoint[Node]): Result[ValueTy] = ???

    private def checkBinary(
      binary: EBinary,
      lhsTy: ValueTy,
      rhsTy: ValueTy,
      expectedTys: Set[ValueTy],
    )(using np: NodePoint[Node]): Unit =
      if (!expectedTys.exists(ty => lhsTy <= ty || rhsTy <= ty))
        val binaryPoint = BinaryOpPoint(np, binary)
        addError(BinaryOpTypeMismatch(binaryPoint, lhsTy, rhsTy))

    /** refine condition */
    override def refine(
      cond: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = cond match {
      // refine boolean local variables
      case ERef(x: Local) =>
        refineBool(x, positive)
      // refine inequality
      case EBinary(BOp.Lt, l, r) =>
        refineIneq(l, r, positive)
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
        refine(e, !positive)
      // refine logical disjunction
      case EBinary(BOp.Or, l, r) =>
        st =>
          if (positive) refine(l, true)(st) ⊔ refine(r, true)(st)
          else refine(r, false)(refine(l, false)(st))
      // refine logical conjunction
      case EBinary(BOp.And, l, r) =>
        st =>
          if (positive) refine(r, true)(refine(l, true)(st))
          else refine(l, false)(st) ⊔ refine(r, false)(st)
      // no pruning
      case _ => st => st
    }

    /** refine types */
    def refine(
      value: AbsValue,
      refinedValue: AbsValue,
    )(using np: NodePoint[_]): Updater =
      import RefinementKind.*
      val refined = refinedValue.ty
      join(for {
        pred <- value.guard.collect {
          case (True, pred) if refined <= TrueT                  => pred
          case (False, pred) if refined <= FalseT                => pred
          case (Normal, pred) if refined <= NormalT              => pred
          case (Abrupt, pred) if refined <= AbruptT              => pred
          case (NormalTrue, pred) if refined <= NormalT(TrueT)   => pred
          case (NormalFalse, pred) if refined <= NormalT(FalseT) => pred
        }
      } yield refine(pred, true))

    /** refine types using symbolic predicates */
    def refine(
      pred: SymExpr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      import SymExpr.*, SymRef.*
      pred match {
        // refine boolean local variables
        case SERef(x: Local) =>
          refineBool(x, positive)
        // refine inequality
        // case SEBinary(BOp.Lt, l, r) =>
        //   refineIneq(l, r, positive)
        // // refine local variables
        // case SEBinary(BOp.Eq, ERef(x: Local), expr) =>
        //   refineLocal(x, expr, positive)
        // // refine field equality
        // case SEBinary(BOp.Eq, ERef(Field(x: Local, EStr(field))), expr) =>
        //   refineField(x, field, expr, positive)
        // // refine field existence
        // case SEExists(Field(x: Local, EStr(field))) =>
        //   refineExistField(x, field, positive)
        // // refine types
        // case SEBinary(BOp.Eq, ETypeOf(ERef(x: Local)), expr) =>
        //   refineType(x, expr, positive)
        // refine type checks
        case SETypeCheck(SERef(ref), ty) =>
          refine(ref, ty, positive)
        // refine logical negation
        case SEUnary(UOp.Not, e) =>
          refine(e, !positive)
        // refine logical disjunction
        case SEBinary(BOp.Or, l, r) =>
          st =>
            if (positive) refine(l, true)(st) ⊔ refine(r, true)(st)
            // TODO short circuiting
            else refine(r, false)(refine(l, false)(st))
        // refine logical conjunction
        case SEBinary(BOp.And, l, r) =>
          st =>
            if (positive) refine(r, true)(refine(l, true)(st))
            // TODO short circuiting
            else refine(l, false)(st) ⊔ refine(r, false)(st)
        // no pruning
        case _ => st => st
      }

    /** refine references using types */
    def refine(
      ref: SymRef,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      import SymExpr.*, SymRef.*
      ref match
        case SSym(sym) => ???
        case SLocal(x) =>
          for {
            l <- transfer(x)
            v <- transfer(l)
            refinedV =
              if (positive)
                if (v.ty <= ty.toValue) v
                else v ⊓ AbsValue(ty)
              else v -- AbsValue(ty)
            _ <- modify(_.update(l, refinedV))
            _ <- refine(v, refinedV) // propagate type guard
          } yield ()
        case SField(base, SEStr(field)) =>
          for {
            bty <- transfer(base)
            rbinding = Binding(ty)
            binding = if (positive) rbinding else bty.record(field) -- rbinding
            refinedTy = ValueTy(
              record = bty.record.update(field, binding, refine = true),
            )
            _ <- refine(base, refinedTy, positive)
          } yield ()
        case _ => st => st

    /** refine types for boolean local variables */
    def refineBool(
      x: Local,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(x)
      lv <- transfer(l)
      refinedV = if (positive) AVT else AVF
      _ <- modify(_.update(l, refinedV))
      _ <- refine(lv, refinedV)
    } yield ()

    /** refine types with inequalities */
    def refineIneq(
      l: Expr,
      r: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater =
      def toLocal(e: Expr): Option[Local] = e match
        case ERef(x: Local) => Some(x)
        case _              => None
      for {
        lv <- transfer(l)
        rv <- transfer(r)
        lmath = lv.ty.math
        rmath = rv.ty.math
        _ <- modify { st =>
          val lst = toLocal(l).fold(st) { x =>
            var math = lmath
            var infinity = lv.ty.infinity --
              (if (positive) InfinityTy.Pos else InfinityTy.Neg)
            val refined = (r, rmath) match
              case (EMath(0), _) =>
                math = if (positive) NegIntTy else NonNegIntTy
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
            )
          }
          toLocal(r).fold(lst) { x =>
            var math = rmath
            var infinity = rv.ty.infinity --
              (if (positive) InfinityTy.Neg else InfinityTy.Pos)
            val refined = (l, lmath) match
              case (EMath(0), _) =>
                math = if (positive) PosIntTy else NonPosIntTy
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
            )
          }
        }
      } yield ()

    /** refine types of local variables with equality */
    def refineLocal(
      x: Local,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      l <- transfer(x)
      lv <- transfer(l)
      refinedV =
        if (positive) lv ⊓ rv
        else if (rv.isSingle) lv -- rv
        else lv
      _ <- modify(_.update(l, refinedV))
      _ <- refine(lv, refinedV)
    } yield ()

    /** TODO refine types with field equality */
    def refineField(
      x: Local,
      field: String,
      expr: Expr,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      rv <- transfer(expr)
      _ <- refineField(x, field, Binding(rv.ty), positive)
    } yield ()

    def refineField(
      x: Local,
      field: String,
      rbinding: Binding,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(x)
      lv <- transfer(l)
      lty = lv.ty
      binding = if (positive) rbinding else lty.record(field) -- rbinding
      refinedTy = ValueTy(
        ast = lty.ast,
        record = lty.record.update(field, binding, refine = true),
      )
      refinedV = AbsValue(refinedTy)
      _ <- modify(_.update(l, refinedV))
      _ <- refine(lv, refinedV)
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
      l <- transfer(x)
      lv <- transfer(l)
      rv <- transfer(expr)
      lty = lv.ty
      rty = rv.ty
      refinedV = rty.str.getSingle match
        case One(tname) =>
          val value = AbsValue(ValueTy.fromTypeOf(tname))
          if (positive) lv ⊓ value else lv -- value
        case _ => lv
      _ <- modify(_.update(l, refinedV))
    } yield ()

    /** refine types with type checks */
    def refineTypeCheck(
      ref: Ref,
      ty: ValueTy,
      positive: Boolean,
    )(using np: NodePoint[_]): Updater = for {
      l <- transfer(ref)
      v <- transfer(l)
      refinedV =
        if (positive)
          if (v.ty <= ty.toValue) v
          else v ⊓ AbsValue(ty)
        else v -- AbsValue(ty)
      _ <- modify(ref match
        case _: Local => _.update(l, refinedV)
        case Field(x: Local, EStr(field)) =>
          refineField(x, field, Binding(ty), positive)
        case _ => identity,
      )
      _ <- refine(v, refinedV)
    } yield ()
  }

  /** use type abstract domains */
  stateDomain = Some(StateTypeDomain(this))
  retDomain = Some(RetTypeDomain)
  valueDomain = Some(ValueTypeDomain)

  /** conversion to string */
  override def toString: String =
    val app = new Appender
    // show detected type errors
    if (detected.nonEmpty)
      app :> "* " >> detected.size
      app >> " type errors are detected."
    // show unused names
    if (unusedSet.nonEmpty)
      app :> "* " >> unusedSet.size
      app >> " names are not used to ignore errors."
    detected.toList.map(_.toString).sorted.map(app :> _)
    // show help message about how to use the ignorance system
    for (path <- ignore.filename)
      app :> "=" * 80
      if (detected.nonEmpty)
        app :> "To suppress this error message, "
        app >> "add the following names to `" >> path >> "`:"
        detected.map(_.func.name).toList.sorted.map(app :> "  + " >> _)
      if (unusedSet.nonEmpty)
        app :> "To suppress this error message, "
        app >> "remove the following names from `" >> path >> "`:"
      unusedSet.toList.sorted.map(app :> "  - " >> _)
      app :> "=" * 80
    app.toString

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** record type errors */
  private def addError(error: TypeError): Unit =
    errorMap += error.point -> error
  private var errorMap: Map[AnalysisPoint, TypeError] = Map()

  /** all entry node points */
  private def getNps(targets: List[Func]): List[NodePoint[Node]] = for {
    func <- targets
    entry = func.entry
    view = getView(func)
  } yield NodePoint(func, entry, view)

  /** get initial abstract states in each node point */
  private def getInitNpMap(
    targets: List[Func],
  ): Map[NodePoint[Node], AbsState] =
    (for {
      np @ NodePoint(func, _, _) <- getNps(targets)
      st = getState(func)
    } yield np -> st).toMap

  /** get view from a function */
  private def getView(func: Func): View = View()

  /** get initial state of function */
  private def getState(func: Func): AbsState =
    val locals = func.params.map {
      case Param(x, ty, _, _) => x -> AbsValue(ty.ty)
    }
    getCalleeState(AbsState.Empty, locals)

  /** get callee state */
  def getCalleeState(
    callerSt: AbsState,
    locals: List[(Local, AbsValue)],
  ): AbsState =
    import SymExpr.*, SymRef.*
    val idxLocals = locals.zipWithIndex
    val (newLocals, symEnv) = (for {
      ((x, value), sym) <- idxLocals
    } yield (
      x -> AbsValue(BotT, One(SERef(SSym(sym))), Map()),
      sym -> value.ty,
    )).unzip
    callerSt
      .setLocal(newLocals.toMap)
      .setSymEnv(symEnv.toMap)

  /** logging the current analysis result */
  def logging: Unit = {
    val analyzedFuncs = sem.analyzedFuncs
    val analyzedNodes = sem.analyzedNodes
    val analyzedReturns = sem.analyzedReturns

    // create log directory
    mkdir(ANALYZE_LOG_DIR)

    // basic logging
    dumpFile(
      name = "summary of type analysis",
      data = Yaml(
        "duration" -> f"${sem.elapsedTime}%,d ms",
        "error" -> errors.size,
        "iter" -> sem.iter,
        "analyzed" -> Map(
          "funcs" -> ratioSimpleString(analyzedFuncs.size, cfg.funcs.size),
          "nodes" -> ratioSimpleString(analyzedNodes.size, cfg.nodes.size),
          "returns" -> ratioSimpleString(analyzedReturns.size, cfg.funcs.size),
        ),
      ),
      filename = s"$ANALYZE_LOG_DIR/summary.yml",
      silent = silent,
    )
    dumpFile(
      name = "type analysis result for each function",
      data = sem.typesString,
      filename = s"$ANALYZE_LOG_DIR/types",
      silent = silent,
    )
    dumpFile(
      name = "visiting counter for control points",
      data = sem.counter.toList
        .sortBy(_._2)
        .map { case (cp, k) => s"[$k] $cp" }
        .mkString(LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/counter",
      silent = silent,
    )
    dumpFile(
      name = "detected type errors",
      data = errors.toList.sorted
        .map(_.toString(detail = true))
        .mkString(LINE_SEP + LINE_SEP),
      filename = s"$ANALYZE_LOG_DIR/errors",
      silent = silent,
    )

    // detailed logging
    if (detail)
      val unreachableDir = s"$ANALYZE_LOG_DIR/unreachable"
      val unreachableFuncs = cfg.funcs.filterNot(analyzedFuncs.contains)
      val unreachableNodes = cfg.nodes.filterNot(analyzedNodes.contains)
      val unreachableReturns = cfg.funcs.filterNot(analyzedReturns.contains)

      // create unreachable directory
      mkdir(unreachableDir)

      dumpFile(
        name = "unreachable functions",
        data = unreachableFuncs.sorted.map(_.nameWithId).mkString(LINE_SEP),
        filename = s"$unreachableDir/funcs",
        silent = silent,
      )
      dumpFile(
        name = "unreachable nodes",
        data = unreachableNodes
          .groupBy(cfg.funcOf)
          .toList
          .sortBy(_._1)
          .map {
            case (f, ns) =>
              f.nameWithId +
              ns.sorted.map(LINE_SEP + "  " + _.name).mkString
          }
          .mkString(LINE_SEP),
        filename = s"$unreachableDir/nodes",
        silent = silent,
      )
      dumpFile(
        name = "unreachable function returns",
        data = unreachableReturns.sorted.map(_.nameWithId).mkString(LINE_SEP),
        filename = s"$unreachableDir/returns",
        silent = silent,
      )
      dumpFile(
        name = "detailed type analysis result for each control point",
        data = sem.resultStrings(detail = true).mkString(LINE_SEP),
        filename = s"$ANALYZE_LOG_DIR/detailed-types",
        silent = silent,
      )
  }
}
object TypeAnalyzer:

  /** algorithm names used in ignoring type errors */
  case class Ignore(
    filename: Option[String] = None,
    names: Set[String] = Set(),
  )
  object Ignore:
    def apply(filename: String): Ignore = Ignore(
      filename = Some(filename),
      names = optional { readJson[Set[String]](filename) }.getOrElse(Set()),
    )

  /** configuration for type checking */
  case class Config(
    checkArity: Boolean = true,
    checkParamType: Boolean = true,
    checkReturnType: Boolean = true,
    checkUncheckedAbrupt: Boolean = false, // TODO
    checkInvalidBase: Boolean = false, // TODO
    checkUnaryOp: Boolean = true,
    checkBinaryOp: Boolean = true,
  )
