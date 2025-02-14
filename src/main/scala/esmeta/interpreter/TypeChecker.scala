// package esmeta.interpreter

// import esmeta.*
// import esmeta.cfg.*
// import esmeta.error.*
// import esmeta.ir.{Func => IRFunc, *, given}
// import esmeta.ty.{*, given}
// import esmeta.state.*
// import scala.annotation.tailrec
// import esmeta.util.BaseUtils.error
// import scala.collection.mutable.{Map => MMap, Set => MSet}

// /** dynamic type checker extension of IR interpreter */
// class TypeChecker(st: State, timeLimit: Option[Int])
//   extends Interpreter(st, false, false, None, timeLimit) {

//   val tyStringifier = TyElem.getStringifier(true, false)
//   import tyStringifier.given

//   // detected type errors while dynamic type checking
//   private val errors: MSet[TypeError] = MSet()
//   protected def addError(error: TypeError): Unit = errors += error

//   // final state with collected type errors
//   lazy val collect: (State, Set[TypeError]) =
//     while (step) {}
//     (st, errors.toSet)

//   // transition for normal instructions (overrided for parameter type checking)
//   override def eval(inst: NormalInst): Unit = inst match {
//     case ret @ IReturn(expr) =>
//       val retVal = eval(expr)
//       val retTy = st.context.func.irFunc.retTy.ty
//       if (retTy.isDefined && !retTy.contains(retVal, st)) {
//         val node = st.context.cursor match
//           case NodeCursor(_, node, _) => node
//           case _                      => error("cursor is not node cursor")
//         val irp = InternalReturnPoint(st.context.func, node, ret)
//         addError(ReturnTypeMismatch(irp, st.typeOf(retVal)))
//       }
//       st.context.retVal = Some(ret, retVal)
//     case _ => super.eval(inst)
//   }

//   // get initial local variables (overrided for return type checking)
//   override def getLocals(
//     params: List[Param],
//     args: List[Value],
//     caller: Call,
//     callee: Callable,
//   ): MMap[Local, Value] = {
//     val func = callee.func
//     val map = MMap[Local, Value]()
//     @tailrec
//     def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
//       case (Nil, Nil) =>
//       case (Param(lhs, ty, optional, _) :: pl, Nil) =>
//         if (optional) aux(pl, Nil)
//         else RemainingParams(ps)
//       case (Nil, args) =>
//         // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
//         callee match
//           case _: Cont =>
//           case _       => throw RemainingArgs(args)
//       case (param :: pl, arg :: al) =>
//         map += param.lhs -> arg
//         val paramTy = param.ty.ty
//         val idx = params.indexOf(param)
//         if (func.isMethod && idx == 0) ()
//         else if (paramTy.isDefined && !paramTy.contains(arg, st))
//           val callPoint = CallPoint(st.context.func, caller, func)
//           val aap = ArgAssignPoint(callPoint, idx)
//           addError(ParamTypeMismatch(aap, st.typeOf(arg)))
//         aux(pl, al)
//     }
//     aux(params, args)
//     map
//   }
// }

// object TypeChecker {
//   def apply(
//     st: State,
//     timeLimit: Option[Int],
//   ): (State, Set[TypeError]) =
//     new TypeChecker(st, timeLimit).collect
// }
