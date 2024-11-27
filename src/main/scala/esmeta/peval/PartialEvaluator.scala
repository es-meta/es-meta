package esmeta.peval

import esmeta.{LINE_SEP, PEVAL_LOG_DIR}
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.{Type => _, *}
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.es.*
import esmeta.peval.pstate.*
import esmeta.peval.util.*
import esmeta.state.{BigInt, *}
import esmeta.spec.{Spec}
import esmeta.util.{Zero, One, Many, Flat}
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import scala.annotation.tailrec
import scala.collection.mutable.{Map as MMap, Set as MSet}
import scala.math.{BigInt as SBigInt}
import scala.util.{Try, Success, Failure}
import esmeta.ir.util.Walker

/** IR PartialEvaluator */
class PartialEvaluator private (
  val program: Program,
  val logPW: Option[PrintWriter] = None,
  val detailPW: Option[PrintWriter] = None,
  val simplifyLevel: Int = 1,
  val timeLimit: Option[Int] = None,
  val renamer: Renamer,
  // NOTE : renamer should NOT be copied when copying PState - renamer is, specializer-level global state.
) {
  import PartialEvaluator.*

  /** given spec */
  given Spec = program.spec

  lazy val funcMap = Map.from(program.funcs.map(p => p.name -> p))
  lazy val getFunc = (fname: String) =>
    funcMap.getOrElse(fname, throw UnknownFunc(fname))

  lazy val forked = MSet.empty[Func];

  def peval(ref: Ref, pst: PState)(using
    MSet[(Var, Var)],
  ): (Predict[RefTarget], Ref) = ref match {
    case x: Var =>
      val newVar = renamer.get(x, pst.context)
      (Known(VarTarget(newVar)), newVar)
    case Field(ref, expr) =>
      val (base, newBase) =
        val (tgt, newRef) = peval(ref, pst);
        (pst(tgt), newRef)
      val (field, newField) = peval(expr, pst)
      val tgt = (base, field) match
        case (Known(b), Known(f)) => Known(FieldTarget(b, f))
        case _                    => (Unknown)
      (tgt, Field(newBase, newField))
  }

  def peval(expr: Expr, pst: PState)(using
    // ad-hoc fix for EClo
    captures: MSet[(Var, Var)],
  ): (Predict[Value], Expr) = {
    val result = expr match {
      case ERef(ref) =>
        val (tgt, newRef) = peval(ref, pst);
        pst(tgt) match
          case Known(v) if v.isLiteralValue => (pst(tgt), v.toExpr)
          case _                            => (pst(tgt), ERef(newRef))
      // TODO : how to properly handle control flow?

      // case EClo(fname, captured) =>
      // val func = cfg.getFunc(fname)
      // (PClo(func, captured.map(x => x -> pst(x)).toMap), expr)

      case EClo(fname, captured) =>
        val func = getFunc(fname)
        // keep original name `x`
        val v = PClo(
          func,
          captured.map(x => x -> pst(renamer.get(x, pst.context))).toMap,
        )
        captures ++= captured.map(old => old -> renamer.get(old, pst.context))
        (Known(v), EClo(fname, captured))

      case e: LiteralExpr =>
        e match
          case EMath(n)        => (Known(Math(n)), e)
          case EInfinity(pos)  => (Known(Infinity(pos)), e)
          case ENumber(double) => (Known(Number(double)), e)
          case EBigInt(bigInt) => (Known(BigInt(bigInt)), e)
          case EStr(str)       => (Known(Str(str)), e)
          case EBool(b)        => (Known(Bool(b)), e)
          case EUndef()        => (Known(Undef), e)
          case ENull()         => (Known(Null), e)
          case EEnum(name)     => (Known(Enum(name)), e)
          case ECodeUnit(c)    => (Known(CodeUnit(c)), e)
      case EList(exprs) =>
        val vs = exprs.map(expr => peval(expr, pst))
        val addr = renamer.newAddr
        pst.allocList(addr, vs.map(_._1).toVector)
        (
          Known(addr),
          EList(
            vs.map((pv, newExpr) =>
              pv match
                case Known(value) => value.toExprOpt.getOrElse(newExpr)
                case Unknown      => newExpr,
            ),
          ),
        )

      case ERecord(tname, pairs) =>
        val addr = renamer.newAddr
        val pvs = for {
          (f, expr) <- pairs
          (pv, newExpr) = peval(expr, pst)
        } yield (f -> pv, f -> newExpr)
        pst.allocRecord(
          addr,
          tname,
          pvs.map(_._1),
        )
        (
          Known(addr),
          ERecord(
            tname,
            pvs.map {
              case ((key, pv), (_, newExpr)) =>
                pv match
                  case Known(value) => key -> value.toExprOpt.getOrElse(newExpr)
                  case Unknown      => key -> newExpr
            },
          ),
        )
      case ESizeOf(expr) =>
        val (pv, newE) = peval(expr, pst)
        pv match
          case Unknown => (Unknown, ESizeOf(newE))
          case Known(v) =>
            v match
              case addr: Addr =>
                pst(addr) match
                  case Unknown => (Unknown, ESizeOf(newE))
                  case Known(po) =>
                    val ret = Math(po.size)
                    (Known(ret), ret.toExpr)
              case Str(s) =>
                val ret = Math(s.length)
                (Known(ret), ret.toExpr)
              case AstValue(ast) =>
                val ret = Math(ast.children.size)
                (Known(ret), ret.toExpr)
              case v => throw InvalidSizeOf(v)

      case EUnary(uop, expr) =>
        val (pv, newExpr) = peval(expr, pst)
        pv match
          case Known(v) =>
            val result = Interpreter.eval(uop, v)
            (
              Known(result),
              result.toExprOpt match
                case None        => EUnary(uop, newExpr)
                case Some(value) => value,
            )
          case Unknown => (Unknown, EUnary(uop, newExpr))

      case EBinary(BOp.And, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(false)) => Known(Bool(false)) -> EBool(false)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.And, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.And, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.And, newLeft, newRight))

      case EBinary(BOp.Or, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(true)) => Known(Bool(true)) -> EBool(true)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.Or, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.Or, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.Or, newLeft, newRight))
      case EBinary(bop, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        val (rv, newRight) = peval(right, pst)
        (lv, rv) match
          case (Known(v1), Known(v2)) =>
            val result = Interpreter.eval(bop, v1, v2)
            (
              Known(result),
              result.toExprOpt.getOrElse(EBinary(bop, newLeft, newRight)),
            )
          case _ => (Unknown, EBinary(bop, newLeft, newRight))

      case ETypeCheck(expr, ty) =>
        val (pv, newExpr) = peval(expr, pst);
        val ret = ty.ty.contains(pv, pst).map(Bool.apply)
        (ret, ret.map((_: Value).toExpr).getOrElse(ETypeCheck(newExpr, ty)))

      case EContains(list, elem) =>
        val (l, newL) = peval(list, pst)
        val (e, newE) = peval(elem, pst) // TODO is this order okay
        l match
          case Unknown => (Unknown, EContains(newL, newE))
          case Known(addr: Addr) =>
            pst(addr) match
              case Unknown => (Unknown, EContains(newL, newE))
              // TODO make two branch linear
              case Known(lobj: PListObj) if (lobj.values.contains(Unknown)) =>
                (Unknown, EContains(newL, newE))
              case Known(lobj: PListObj) =>
                e match
                  case Unknown => (Unknown, EContains(newL, newE))
                  case Known(ev) =>
                    val ret = Bool(lobj.values.contains(Known(ev)))
                    (Known(ret), ret.toExpr)
              case Known(o) =>
                throw UnexpectedKnown(o, "not a list for EContains")
          case Known(v) => throw UnexpectedKnown(v, "not an addr for EContains")

      // case EParse(code, rule)                       => ???
      // case EGrammarSymbol(name, params)             => ???
      // case ESourceText(expr)                        => ???
      // case EYet(msg)                                => ???
      // case ESubstring(expr, from, to)               => ???
      // case ETrim(expr, isStarting)                  => ???

      // case EVariadic(vop, exprs)                    => ???
      // case EMathOp(mop, args)                       => ???
      // case EConvert(cop, expr)                      => ???
      // case EExists(ref)                             => ???
      // case ETypeOf(base)                            => ???
      // case EInstanceOf(base, target)                => ???
      // case ETypeCheck(base, ty)                     => ???

      // case ECont(fname)                             => ???
      // case EDebug(expr)                             => ???
      // case ERandom()                                => ???
      // case ESyntactic(name, args, rhsIdx, children) => ???
      // case ELexical(name, expr)                     => ???
      // case ERecord(tname, pairs)                    => ???
      // case EMap(ty, pairs)                          => ???
      // case ECopy(obj)                               => ???
      // case EKeys(map, intSorted)                    => ???

      case _ =>
        logging("expr", s"NOT SUPPORTED EXPR! $expr")
        (Unknown, RenameWalker(expr)(using renamer, pst))
    }
    logging("expr", s"$expr -> $result")
    val (v, newE) = result
    if (detailPW.isDefined && !v.isKnownLiteral) then
      (v, newE.appendCmt(v.toString(detail = true)))
    else (v, newE)
  }

  def peval(inst: Inst, pst: PState)(using
    captures: MSet[(Var, Var)],
  ): (Inst, PState) = {
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      inst.toString(detail = false),
    )
    val (newInst, newPst) = inst match
      case ILet(lhs, expr) =>
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newExpr) = peval(expr, pst)
        pst.define(newLhs, pv)
        (ILet(newLhs, newExpr), pst)
      case ISeq(insts) =>
        val (newInsts, newPst) = insts.foldLeft((List[Inst](), pst)) {
          case ((acc, pst), inst) =>
            val (newInst, newPst) = peval(inst, pst)
            (acc :+ newInst, newPst)
        }
        (ISeq(newInsts), newPst)

      case ISdoCall(lhs, base, method, args) =>
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newBase) = peval(base, pst)
        val vs = args.map((e) => peval(e, pst)) // TODO check order
        pv match
          case Unknown =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          case Known(ast: AstValue) if vs.map(_._1).exists(_.isEmpty) =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          // TODO: local variable inline: <varname>_<fid>_<ctxtcounter>
          // case Known(AstValue(ast)) =>
          case Known(value) =>
            value.asAst match {
              case syn: Syntactic => {
                val (ast0, sdo) = getSdo((syn, method)).getOrElse(
                  throw InvalidAstField(syn, Str(method)),
                )

                sdocallClo(
                  sdo: Func,
                  newBase: Expr,
                  method: String,
                  vs: List[(Predict[Value], Expr)],
                  pst: PState,
                  ast0: Ast,
                  newLhs: Local,
                  newCallCount: Int,
                  syn: Syntactic,
                )
              }

              case lex: Lexical =>
                val v = Interpreter.eval(lex, method);
                pst.define(newLhs, Known(v))
                (IAssign(newLhs, v.toExpr), pst)
            }
      case call @ ICall(lhs, fexpr, args) =>
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (f, newFexpr) = peval(fexpr, pst)
        val vs = args.map(e => peval(e, pst)) // TODO check order
        {
          f match
            case Known(pclo @ PClo(_, _))
                if shouldComputeCall(call, pclo, vs) =>
              callClo(call, pclo, newFexpr, vs, pst)
            case Known(_: PClo) /* with Unknown argument */ =>
              pst.define(newLhs, Unknown)
              pst.heap.clear(vs.map(_._1))
              (ICall(newLhs, newFexpr, vs.map(_._2)), pst)
            case Known(_: PCont) => throwPeval"not yet supported"
            case Known(v)        => throw NoCallable(v)
            case Unknown =>
              pst.define(newLhs, Unknown)
              pst.heap.clear(vs.map(_._1))
              (ICall(newLhs, newFexpr, vs.map(_._2)), pst)
        }

      case INop() => (INop(), pst)
      case IAssign(ref, expr) =>
        ref match
          case x: Var =>
            val newVar = renamer.get(x, pst.context);
            val (pv, newExpr) = peval(expr, pst)
            pst.update(newVar, pv)
            (IAssign(newVar, newExpr), pst)
          case Field(_, _) =>
            pst.heap.clear
            (RenameWalker(inst)(using renamer, pst), pst) // TODO

      case IPop(lhs, list, front) =>
        val newLhs = renamer.get(lhs, pst.context);
        val (pv, newListExpr) = peval(list, pst);
        pv match
          case Known(_) => pst.heap.clear // TODO : modify heap
          case Unknown  => pst.heap.clear // TODO : kill heap
        pst.define(newLhs, Unknown)
        (IPop(newLhs, newListExpr, front), pst)

      case IPush(elem, list, front) =>
        val (value, newElem) = peval(elem, pst)
        val (pv, newListExpr) = peval(list, pst)
        pv match
          case Known(addr: Addr) => pst.heap.push(addr, value, front)
          case Unknown           => pst.heap.clear // TODO : kill heap
          case _                 => throwPeval"not an address"
        (IPush(newElem, newListExpr, front), pst)

      case IReturn(expr) =>
        val (pv, newExpr) = peval(expr, pst);
        pst.context.ret ||= pv.toFlat
        // NOTE .toFlat, and use Flat[Value] instead of Flat[Predict[Value]], to handle situations like One(Unknown).
        (
          // IAssign(head.retId, newExpr)
          IReturn(newExpr)
            .appendCmt(s"<~ IReturn @ ${pst.context.func.name}"),
          pst,
        )

      case iif @ IIf(cond, thenInst, elseInst) =>
        val (pv, newCond) = peval(cond, pst)
        pv match
          case Known(Bool(true))                 => peval(thenInst, pst)
          case Known(Bool(false))                => peval(elseInst, pst)
          case Known(v)                          => throw NoBoolean(v)
          case Unknown if pst.callStack.size > 0 => throw NoMoreInline()
          case Unknown /* pst.callStack.size == 0 */ => {

            val (thenPst, elsePst) = (pst.copied, pst.copied)
            val (newThen, newThenPst) = peval(thenInst, thenPst)
            val (newElse, newElsePst) = peval(elseInst, elsePst)
            val newPst = newThenPst.join(elsePst)
            (IIf(newCond, newThen, newElse), newPst)

            /* Handle Return */
          }
      case IExpr(expr) => (IExpr(peval(expr, pst)._2), pst)
      case IExpand(_, _) | IDelete(_, _) => /* heap modifying insts */
        val newInst = RenameWalker(inst)(using renamer, pst)
        pst.heap.clear
        (newInst.appendCmt("not supported yet"), pst)

      case IAssert(expr) =>
        val (pv, newExpr) = peval(expr, pst);
        val newInst = IAssert(newExpr);
        pv match
          case Known(v) =>
            v.asBool match
              case true  => (INop(), pst)
              case false => throw AssertionFail(expr)
          case Unknown =>
            (newInst.appendCmt("will be checked at runtime."), pst)

      case IPrint(expr) =>
        val (_, newExpr) = peval(expr, pst);
        val newInst = IPrint(newExpr);
        (newInst, pst)

      case iwhile @ IWhile(cond, body) =>
        val (cv, _) = peval(cond, pst)
        cv match
          case Known(Bool(false)) => (INop(), pst)
          case Known(Bool(true)) =>
            val (newBody, newPst) = peval(body, pst)
            val (rest, newPst2) = peval(iwhile, newPst)
            (ISeq(newBody :: rest :: Nil), newPst2)
          case Known(v) => throw NoBoolean(v)
          case Unknown =>
            val killRet = {

              extension [T](ft: Flat[T]) {
                def <(other: Flat[T]): Boolean =
                  ft <= other && (!(other <= ft))
              }

              val (_, newPst) = peval(body, pst.copied)
              pst.context.ret < newPst.context.ret

            }
            if (killRet) then pst.context.ret = Many;
            val affectedLocals =
              LocalImpact(body)(using renamer, pst.context)
            for { l <- affectedLocals } pst.define(l, Unknown)
            pst.heap.clear
            (
              RenameWalker(inst)(using renamer, pst).appendCmt(
                "unknown condition : heap is cleared",
              ),
              pst,
            )
    logging("pst", pst)
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      s"${inst.toString(detail = false)} -> ${newInst.toString(detail = true).replace("\n", " ")}\n",
    )
    captures.size match
      case 0 => (newInst, newPst)
      case _ =>
        val assigns = ISeq(captures.toList.map {
          case (old -> newV) => IAssign(old, ERef(newV))
        })
        captures.clear
        (ISeq(assigns :: newInst :: Nil), newPst)
  }

  /* decide whether to compute call or skip and use Unknown */
  private def shouldComputeCall(
    callInst: ICall | ISdoCall,
    pclo: PClo,
    vs: Iterable[(Predict[Value], _)],
  ): Boolean = {
    callInst match
      case ISdoCall(_, _, _, _) => true
      case ICall(_, _, _)       => vs.forall(_._1.isDefined)
  }

  private def sdocallClo(
    sdo: Func,
    newBase: Expr,
    method: String,
    vs: List[(Predict[Value], Expr)],
    pst: PState,
    ast0: Ast,
    newLhs: Local,
    newCallCount: Int,
    syn: Syntactic,
  )(using captures: MSet[(Var, Var)]): (Inst, PState) = {

    val calleeCtx = PContext(
      func = sdo,
      locals = MMap.empty,
      sensitivity = newCallCount,
      ret = Zero,
    );

    val baseRep = renamer.newTempCount;
    val subpath = AstHelper.getSubgraphPath(syn, ast0);

    val allocations =
      (IAssign(Temp(baseRep), newBase)) :: setLocals[PClo](
        at = calleeCtx,
        params = sdo.params.map(p =>
          Param(
            renamer.get(p.lhs, calleeCtx),
            p.ty,
            p.optional,
            p.specParam,
          ),
        ),

        // TODO : There is no way to print ast0 as expression this should be removed somehow
        /* Ad-hoc fix */

        args = (Known(AstValue(ast0)) -> ERef(
          subpath.foldLeft[Ref](Temp(baseRep))((acc, idx) =>
            Field(acc, EMath(idx)),
          ),
        )) :: vs,
        func = sdo,
      )

    val calleePst = {
      val fresh = pst.copied;
      fresh.callStack ::= PCallContext(
        pst.context, /* , newLhs */
      )
      fresh.context = calleeCtx
      fresh
    }

    Try {
      val (body, after) = peval(sdo.body, calleePst)
      after.context.ret match
        case Zero => { throw NoReturnValue }
        case One(elem) => {
          after.callStack match
            case Nil => /* never */ ???
            case callerCtx :: rest =>
              val calleeCtx = after.context;
              after.callStack = rest
              after.context = callerCtx.ctxt
              after.define(
                newLhs,
                calleeCtx.ret.toPredict(throw NoReturnValue),
              )
              val substitutor = new Walker {
                override def walk(inst: Inst): Inst = inst match
                  case IReturn(expr) => IAssign(newLhs, expr)
                  case _             => super.walk(inst)
              }
              (ISeq(List(ISeq(allocations), substitutor.walk(body))), after)
        }
        case Many => {
          // TODO add fork

          /*
          val forked = Func(
                      sdo.main,
                      sdo.kind,
                      s"${sdo.name}Forked${PartialEvaluator.getForkId}", // how ??
                      sdo.params, // how??
                      sdo.retTy,
                      ISeq(List(body)),
                      Some(sdo.name),
                      sdo.algo,
                    )

                    this.forked += forked

                    pst.define(newLhs, Unknown)
                    pst.heap.clear(vs.map(_._1)) // ...
                    (
                      ISdoCall(
                        newLhs,
                        newBase,
                        method // how??
                        ,
                        vs.map(_._2),
                      ),
                      pst,
                    )
           */
          ???
        }
    }.get
  }

  private def callClo(
    icall: ICall,
    pclo: PClo,
    newFexpr: Expr,
    vs: List[(Predict[Value], Expr)],
    pst: PState,
  )(using
    captures: MSet[(Var, Var)],
  ): (Inst, PState) = {
    /* invariant: function is evaluated */
    /* invariant: every arguments are known */
    val ICall(lhs, _, args) = icall
    val PClo(callee, captured) = pclo
    val newCallCount = renamer.newCallCount
    val newLhs = renamer.get(lhs, pst.context)

    val calleeCtx = PContext(
      func = callee,
      locals = MMap.empty,
      sensitivity = newCallCount,
      ret = Zero,
    );

    val allocations = setLocals[PClo](
      at = calleeCtx,
      params = callee.params.map(p =>
        Param(
          renamer.get(p.lhs, calleeCtx),
          p.ty,
          p.optional,
          p.specParam,
        ),
      ),

      // TODO : There is no way to print ast0 as expression this should be removed somehow
      /* Ad-hoc fix */
      args = vs,
      func = callee,
    )

    val calleePst = {
      val fresh = pst.copied;
      fresh.callStack ::= PCallContext(pst.context /* , newLhs */ )
      fresh.context = calleeCtx
      fresh
    }

    Try {
      val (body, after) = peval(callee.body, calleePst)
      after.context.ret match
        case Zero => throw NoReturnValue
        case One(elem) => {
          after.callStack match
            case Nil => /* never */ ???
            case callerCtx :: rest =>
              val calleeCtx = after.context;
              after.callStack = rest
              after.context = callerCtx.ctxt
              val retVal = calleeCtx.ret
                .toPredict(throw NoReturnValue)
              after.define(newLhs, retVal)
              val substitutor = new Walker {
                override def walk(inst: Inst): Inst = inst match
                  case IReturn(expr) => IAssign(newLhs, expr)
                  case _             => super.walk(inst)
              }
              (ISeq(List(ISeq(allocations), substitutor.walk(body))), after)
        }
        case Many => {

          after.callStack match
            case Nil => /* never */ ???
            case callerCtx :: rest =>
              val calleeCtx = after.context;
              after.callStack = rest
              after.context = callerCtx.ctxt
              val retVal = calleeCtx.ret
                .toPredict(throw NoReturnValue)
              after.define(newLhs, retVal)
              val substitutor = new Walker {
                override def walk(inst: Inst): Inst = inst match
                  case IReturn(expr) => IAssign(newLhs, expr)
                  case _             => super.walk(inst)
              }

              val newName = s"${callee.name}Forked${PartialEvaluator.getForkId}"

              val forked = Func(
                callee.main,
                callee.kind,
                newName,
                callee.params,
                callee.retTy,
                ISeq(List(body)),
                Some(callee.name),
                callee.algo,
              )

              this.forked += forked

              pst.define(newLhs, Unknown)
              pst.heap.clear(vs.map(_._1)) // ...
              // fork하면 -> 새 함수 이름을 만들어서 그걸 부르도록 하자!
              // 생각해보니 -> ICall 함수 이름이 아니라 함수 값 (클로저 값)을 들고 있다.
              // 간단한 케이스만 하거나 ? 아예 이름으로 구분하지 않게 하거나?
              val newFExpr = EClo(newName, ???) // ???
              (ICall(newLhs, newFExpr, vs.map(_._2)), after)
        }
    }.recoverWith {
      case NoMoreInline() =>
        pst.define(newLhs, Unknown)
        pst.heap.clear(vs.map(_._1))
        Success(ICall(newLhs, newFexpr, vs.map(_._2)), pst)
    }.get
  }

  /** final state */
  def compute(
    func: Func,
    pst: PState,
    newName: Option[String] = None,
  ): (Func, PState) = timeout(
    {
      val inst = func.body
      val newParams = func.params.map {
        case Param(lhs, ty, optional, specParam) =>
          Param(renamer.get(lhs, pst.context), ty, optional, specParam)
      }
      val result @ (newBody, newPst) = peval(inst, pst)(using MSet.empty)
      val simplifiedNewBody = simplifyLevel match
        case 0 => newBody
        case 1 => IRSimplifier.flat(newBody)
        case 2 => IRSimplifier.flat(IRSimplifier.syntactic(newBody))
        case 3 => ??? // InstFlattener(NoLiterals(newBody)) // TODO

      (
        Func(
          func.main,
          func.kind,
          newName.getOrElse(func.name),
          newParams,
          func.retTy,
          simplifiedNewBody,
          Some(func.name),
          func.algo,
        ),
        newPst,
      )
    },
    timeLimit,
  )

  /** set initial local variables */
  private def setLocals[T <: PCallable](
    at: PContext,
    params: List[Param],
    args: List[(Predict[Value], Expr)],
    func: Func,
  )(using ev: T <:< PCont = null): List[Inst] = {
    val map = at.locals;
    @tailrec
    def aux(
      evalArgs: List[Inst],
    )(ps: List[Param], as: List[(Predict[Value], Expr)]): List[Inst] =
      (ps, as) match {
        case (Nil, Nil) => (evalArgs)
        case (Param(lhs, ty, optional, _) :: pl, Nil) =>
          if (optional) aux(evalArgs)(pl, Nil)
          else throw RemainingParams(ps)
        case (Nil, args) =>
          // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
          if (ev != null) then (evalArgs) else ??? // throw RemainingArgs(args)
        case (param :: pl, (arg, argExpr) :: al) =>
          map += (param.lhs -> arg)
          aux(
            IAssign(param.lhs, argExpr) :: evalArgs,
          )(
            pl,
            al,
          )
      }
    // reverse needed to keep order
    aux(Nil)(params, args).reverse.toList
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  private val logging = (tag: String, msg: Any) =>
    logPW.foreach { pw =>
      pw.println(s"[$tag] $msg"); pw.flush
    }

  private val detailed = (tag: String, msg: Any) =>
    logPW.foreach { pw =>
      pw.println(s"[$tag] $msg"); pw.flush
    }

  /** cache to get syntax-directed operation (SDO) */
  private val getSdo =
    cached[(Ast, String), Option[(Ast, Func)]](
      _.getSdo[Func](_)(using program.spec, funcMap),
    )

  object Logger {
    def logForked(f: Func): Unit = {
      logPW.foreach { pw =>
        pw.println(s"[FORKED] ${f.name}"); pw.flush
      }
    }
  }
}

/** IR PartialEvaluator */
object PartialEvaluator {

  private def getForkId: Int = GlobalRenamer.getFuncId

  /** create new Renamer and PState for partial evaluation */
  private def createCleanState(func: Func): (Renamer, PState) = {
    val renamer = Renamer()
    val thisCallCount = renamer.newCallCount
    val pst = PState.empty(
      PContext(
        func,
        sensitivity = thisCallCount,
        locals = MMap(),
        ret = Zero,
      ),
    );

    (renamer, pst)
  }

  /* run partial evaluation
   *
   * @param program ir program (set of functions)
   * @param func target function to evaluate
   * @param initialize state initializer to create proper state for func
   *
   * @return (result function, additionally forked functions)
   */
  def run(program: Program, func: Func)(initialize: (Renamer, PState) => Unit)(
    logPW: Option[PrintWriter] = None,
    detailPW: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    simplifyLevel: Int = 0,
  ): (Func, List[Func]) = {
    val (renamer, pst) = createCleanState(func)
    initialize(renamer, pst)
    val pev = PartialEvaluator(
      program = program,
      logPW = logPW,
      detailPW = detailPW,
      simplifyLevel = simplifyLevel,
      timeLimit = timeLimit,
      renamer = renamer,
    )
    val result = pev.compute(func, pst, None)._1
    (result, pev.forked.toList)
  }

}
