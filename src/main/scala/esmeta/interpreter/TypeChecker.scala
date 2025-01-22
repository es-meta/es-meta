package esmeta.interpreter

import esmeta.*
import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.*
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** dynamic type checker extension of IR interpreter */
class TypeChecker(st: State, target: Option[String] = None)
  extends Interpreter(st) {
  import TypeChecker.*

  // mismatches while dynamic type checking
  val mismatches: MSet[TypeMismatch] = MSet()

  // transition for normal instructions (overrided for parameter type checking)
  override def eval(inst: NormalInst): Unit = inst match {
    case ret @ IReturn(expr) =>
      val retVal = eval(expr)
      val retTy = st.context.func.irFunc.retTy.ty
      if (retTy.isDefined && !retTy.contains(retVal, st)) {
        mismatches += TypeMismatch(
          "ReturnTypeMismatch",
          st.context.func.irFunc.name,
          None,
          target,
        )
      }
      st.context.retVal = Some(ret, retVal)
    case _ => super.eval(inst)
  }

  // get initial local variables (overrided for return type checking)
  override def getLocals(
    params: List[Param],
    args: List[Value],
    caller: Call,
    callee: Callable,
  ): MMap[Local, Value] = {
    val func = callee.func
    val map = MMap[Local, Value]()
    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
      case (Nil, Nil) =>
      case (Param(lhs, ty, optional, _) :: pl, Nil) =>
        if (optional) aux(pl, Nil)
        else RemainingParams(ps)
      case (Nil, args) =>
        // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
        callee match
          case _: Cont =>
          case _       => throw RemainingArgs(args)
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        val paramTy = param.ty.ty
        if (paramTy.isDefined && !paramTy.contains(arg, st)) {
          val thisMethodCall = func.isMethod && params.indexOf(param) == 0
          if (!thisMethodCall) {
            mismatches += TypeMismatch(
              "ParamTypeMismatch",
              func.irFunc.name,
              Some(param.lhs.name),
              target,
            )
          }
        }
        aux(pl, al)
    }
    aux(params, args)
    map
  }
}

object TypeChecker {
  case class TypeMismatch(
    tag: String, // "ParamTypeMismatch" or "ReturnTypeMismatch"
    algo: String,
    param: Option[String], // Defined if tag is "ParamTypeMismatch"
    source: Option[String],
  )

  case class ChunkedTypeMismatch(
    tag: String, // "ParamTypeMismatch" or "ReturnTypeMismatch"
    algo: String,
    param: Option[String], // Defined if tag is "ParamTypeMismatch"
    sources: Set[String],
  )

  object TypeMismatch {
    def chunk(mismatches: MSet[TypeMismatch]): Seq[ChunkedTypeMismatch] =
      mismatches
        .groupBy(m => (m.tag, m.algo, m.param))
        .map {
          case ((tag, algo, param), ms) =>
            ChunkedTypeMismatch(tag, algo, param, ms.flatMap(_.source).toSet)
        }
        .toSeq
        .sortBy(-_.sources.size)
  }
}
