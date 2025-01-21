package esmeta.interpreter

import esmeta.*
import esmeta.TEST_MODE
import esmeta.cfg.*
import esmeta.error.*
import esmeta.error.NotSupported.Category.{Type => _, *}
import esmeta.error.NotSupported.{*, given}
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.state.*
import esmeta.spec.{
  SyntaxDirectedOperationHead,
  AbstractOperationHead,
  BuiltinHead,
}
import esmeta.ty.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.{EVAL_LOG_DIR, LINE_SEP}
import java.io.PrintWriter
import java.math.MathContext.DECIMAL128
import java.util.concurrent.TimeoutException
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.math.{BigInt => SBigInt}

/** extensible helper of IR interpreter with a CFG */
class Interpreter(
  val st: State,
  val log: Boolean = false,
  val detail: Boolean = false,
  val logPW: Option[PrintWriter] = None,
  val timeLimit: Option[Int] = None,
  val tyCheck: Boolean = false,
) {
  import Interpreter.*

  /** initial time */
  lazy val startTime: Long = System.currentTimeMillis

  /** iteration cycle */
  lazy val ITER_CYCLE: Int = 100_000

  /** final state */
  lazy val result: State =
    while (step) {}
    if (tyCheck && mismatches.nonEmpty)
      for (mismatch <- mismatches) {
        println(LINE_SEP)
        println(s"[${mismatch.tag}] ${mismatch.algo}")
        if (mismatch.param.isDefined) println(s"- param: ${mismatch.param.get}")
        if (mismatch.source.isDefined) println(s"--- ${mismatch.source.get}")
      }
      println(LINE_SEP)
    if (log)
      pw.println(st)
      pw.close
      println("[Interpreter] Logging finished")
    st

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser

  /** step */
  def step: Boolean =
    try {
      // text-based logging
      if (log)
        pw.println(st.getCursorString)
        if (detail) pw.println(st.context)
        pw.flush

      // garbage collection
      iter += 1
      if (iter % ITER_CYCLE == 0) {
        for (limit <- timeLimit)
          val duration = System.currentTimeMillis - startTime
          if (duration / 1000 > limit) throw TimeoutException("interp")
        if (!detail) GC(st)
      }

      // cursor
      eval(st.context.cursor)
    } catch {
      case e =>
        if (log)
          pw.println(st)
          pw.println("[Interpreter] unexpected error: " + e)
          pw.println(e.getStackTrace.mkString(LINE_SEP))
          pw.flush
        throw e
    }

  /** transition for cursors */
  def eval(cursor: Cursor): Boolean = cursor match
    case NodeCursor(_, node, _) => eval(node); true
    case ExitCursor(func) =>
      st.callStack match
        case Nil =>
          st.context.retVal.map((_, v) => st.globals += GLOBAL_RESULT -> v)
          false
        case CallContext(ctxt, retId) :: rest =>
          val (ret, value) = st.context.retVal.getOrElse(throw NoReturnValue)
          st.context = ctxt
          st.callStack = rest
          setCallResult(retId, value)
          true

  /** transition for nodes */
  def eval(node: Node): Unit =
    node match {
      case Block(_, insts, _) =>
        for (inst <- insts) {
          eval(inst)
          st.context.moveInst
        }
        st.context.moveNode
      case branch: Branch =>
        eval(branch.cond) match
          case Bool(bool) => moveBranch(branch, bool)
          case v          => throw NoBoolean(v)
      case call: Call => eval(call)
    }

  /** transition for normal instructions */
  def eval(inst: NormalInst): Unit = inst match {
    case IExpr(expr)         => eval(expr)
    case ILet(lhs, expr)     => st.define(lhs, eval(expr))
    case IAssign(ref, expr)  => st.update(eval(ref), eval(expr))
    case IExpand(base, expr) => st.expand(st(eval(base)), eval(expr))
    case IDelete(base, expr) => st.delete(st(eval(base)), eval(expr))
    case IPush(elem, list, front) =>
      val value = eval(elem)
      val addr = eval(list).asAddr
      st.push(addr, value, front)
    case IPop(lhs, list, front) =>
      val addr = eval(list).asAddr
      st.context.locals += lhs -> st.pop(addr, front)
    case ret @ IReturn(expr) =>
      val retVal = eval(expr)
      val retTy = st.context.func.irFunc.retTy.ty
      if (tyCheck && retTy.isDefined) {
        if (!retTy.contains(retVal, st)) {
          mismatches += TypeMismatch(
            "ReturnTypeMismatch",
            st.context.func.irFunc.name,
            None,
            getSource(st),
          )
        }
      }
      st.context.retVal = Some(ret, retVal)
    case IAssert(expr) =>
      optional(eval(expr)) match
        case None             => /* skip not yet compiled assertions */
        case Some(Bool(true)) =>
        case v                => throw AssertionFail(expr)
    case IPrint(expr) =>
      val v = eval(expr)
      if (!TEST_MODE) println(st.getString(v))
    case INop() => /* do nothing */
  }

  /** transition for calls */
  def eval(call: Call): Unit = call.callInst match {
    case ICall(lhs, fexpr, args) =>
      eval(fexpr) match
        case clo @ Clo(func, captured) =>
          val vs = args.map(eval)
          val newLocals =
            getLocals(func.irFunc.params, vs, call, clo) ++ captured
          st.callStack ::= CallContext(st.context, lhs)
          st.context = createContext(call, func, newLocals, st.context)
        case cont @ Cont(func, captured, callStack) => {
          val vs = args.map(eval)
          val newLocals =
            getLocals(func.irFunc.params, vs, call, cont) ++ captured
          st.callStack = callStack.map(_.copied)
          val prevCtxt = st.callStack.headOption.map(_.context)
          st.context = createContext(call, func, newLocals, prevCtxt)
        }
        case v => throw NoCallable(v)
    case ISdoCall(lhs, base, method, args) =>
      eval(base).asAst match
        case syn: Syntactic =>
          getSdo((syn, method)) match
            case Some((ast0, sdo)) =>
              val vs = args.map(eval)
              val newLocals = getLocals(
                sdo.irFunc.params,
                AstValue(ast0) :: vs,
                call,
                Clo(sdo, Map()),
              )
              st.callStack ::= CallContext(st.context, lhs)
              st.context = createContext(call, sdo, newLocals, st.context)
            case None => throw InvalidAstField(syn, Str(method))
        case lex: Lexical =>
          setCallResult(lhs, Interpreter.eval(lex, method))
  }

  /** transition for expressions */
  def eval(expr: Expr): Value = expr match {
    case EParse(code, rule) =>
      val (str, args, locOpt) = eval(code) match
        case Str(s) => (s, List(), None)
        case AstValue(syn: Syntactic) =>
          (syn.toString(grammar = Some(grammar)), syn.args, syn.loc)
        case AstValue(lex: Lexical) => (lex.str, List(), lex.loc)
        case v                      => throw InvalidParseSource(code, v)
      try {
        (str, eval(rule).asGrammarSymbol, st.sourceText, st.cachedAst) match
          // optimize the initial parsing using the given cached AST
          case (x, GrammarSymbol("Script", Nil), Some(y), Some(ast))
              if x == y =>
            AstValue(ast)
          case (x, GrammarSymbol(name, params), _, _) =>
            val ast =
              esParser(name, if (params.isEmpty) args else params).from(x)
            // TODO handle span of re-parsed ast
            ast.clearLoc
            ast.setChildLoc(locOpt)
            AstValue(ast)
      } catch {
        case _: Throwable => st.allocList(Nil) // NOTE: throw a List of errors
      }
    case EGrammarSymbol(name, params) => GrammarSymbol(name, params)
    case ESourceText(expr) =>
      val ast = eval(expr).asAst
      // XXX fix last space in ECMAScript stringifier
      Str(ast.toString(grammar = Some(grammar)).trim)
    case EYet(msg) =>
      throw NotSupported(Metalanguage)(List(msg))
    case EContains(list, elem) =>
      val l = eval(list).asList(st)
      val e = eval(elem)
      Bool(l.values.contains(e))
    case ESubstring(expr, from, to) =>
      val s = eval(expr).asStr
      val f = eval(from).asInt
      Str(to.fold(s.substring(f))(eval(_) match
        case Math(n) if s.length < n => s.substring(f)
        case v                       => s.substring(f, v.asInt),
      ))
    case ETrim(expr, isStarting) =>
      Str(trimString(eval(expr).asStr, isStarting, esParser))
    case ERef(ref) =>
      st(eval(ref))
    case EUnary(uop, expr) =>
      val x = eval(expr)
      Interpreter.eval(uop, x)
    case EBinary(BOp.And, left, right) => shortCircuit(BOp.And, left, right)
    case EBinary(BOp.Or, left, right)  => shortCircuit(BOp.Or, left, right)
    case EBinary(bop, left, right) =>
      val l = eval(left)
      val r = eval(right)
      Interpreter.eval(bop, l, r)
    case EVariadic(vop, exprs) =>
      val vs = for (e <- exprs) yield eval(e)
      Interpreter.eval(vop, vs)
    case EMathOp(mop, exprs) =>
      val vs = for (e <- exprs) yield eval(e)
      Interpreter.eval(mop, st, vs)
    case EConvert(cop, expr) =>
      import COp.*
      (eval(expr), cop) match {
        // code unit
        case (CodeUnit(c), ToMath) => Math(c.toInt)
        // extended mathematical value
        case (Infinity(true), ToNumber)  => NUMBER_POS_INF
        case (Infinity(false), ToNumber) => NUMBER_NEG_INF
        case (Math(n), ToApproxNumber)   => Number(n.toDouble) // TODO
        case (Math(n), ToNumber)         => Number(n.toDouble)
        case (Math(n), ToBigInt)         => BigInt(n.toBigInt)
        case (Math(n), ToMath)           => Math(n)
        // string
        case (Str(s), ToNumber) => ESValueParser.str2number(s)
        case (Str(s), ToBigInt) => ESValueParser.str2bigint(s)
        case (Str(s), _: ToStr) => Str(s)
        // numbers
        case (Number(d), ToMath) => Math(d)
        case (Number(d), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(toStringHelper(d, radix))
        case (Number(d), ToNumber) => Number(d)
        case (Number(n), ToBigInt) => BigInt(BigDecimal.exact(n).toBigInt)
        // big integer
        case (BigInt(n), ToMath) => Math(n)
        case (BigInt(n), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(n.toString(radix))
        case (BigInt(n), ToBigInt) => BigInt(n)
        // invalid cases
        case (v, cop) => throw InvalidConversion(cop, expr, v)
      }
    case EExists(ref) => Bool(st.exists(eval(ref)))
    case ETypeOf(base) =>
      Str(eval(base) match
        case n: Number => "Number"
        case b: BigInt => "BigInt"
        case s: Str    => "String"
        case b: Bool   => "Boolean"
        case Undef     => "Undefined"
        case Null      => "Null"
        case v =>
          if (ObjectT.contains(v, st)) "Object"
          else if (SymbolT.contains(v, st)) "Symbol"
          else "SpecType",
      )
    case EInstanceOf(expr, target) =>
      (eval(expr), eval(target)) match
        case (AstValue(_: Syntactic), GrammarSymbol("", _)) => Bool(true)
        case (AstValue(ast), GrammarSymbol(name, _)) => Bool(ast.name == name)
        case _                                       => Bool(false)
    case ETypeCheck(expr, ty) =>
      Bool(ty.ty.contains(eval(expr), st))
    case ESizeOf(expr) =>
      Math(eval(expr) match
        case Str(s)        => s.length
        case addr: Addr    => st(addr).size
        case AstValue(ast) => ast.children.size
        case v             => throw InvalidSizeOf(v),
      )
    case EClo(fname, captured) =>
      val func = cfg.getFunc(fname)
      Clo(func, captured.map(x => x -> st(x)).toMap)
    case ECont(fname) =>
      val func = cfg.getFunc(fname)
      val captured = st.context.locals.collect { case (x: Name, v) => x -> v }
      Cont(func, captured.toMap, st.callStack)
    case EDebug(expr) => debug(eval(expr))
    case ERandom()    => Number(math.random)
    case ESyntactic(name, args, rhsIdx, children) =>
      val asts = children.map(_.map(child => eval(child).asAst))
      AstValue(Syntactic(name, args, rhsIdx, asts))
    case ELexical(name, expr) => AstValue(Lexical(name, eval(expr).asStr))
    case ERecord(tname, fields) =>
      st.allocRecord(tname, for ((f, expr) <- fields) yield f -> eval(expr))
    case EMap(_, pairs) =>
      st.allocMap(for ((k, v) <- pairs) yield eval(k) -> eval(v))
    case EList(exprs) => st.allocList(exprs.map(expr => eval(expr)).toVector)
    case ECopy(obj)   => st.copy(eval(obj).asAddr)
    case EKeys(map, intSorted) => st.keys(eval(map).asAddr, intSorted)
    case EMath(n)              => Math(n)
    case EInfinity(pos)        => Infinity(pos)
    case ENumber(n) if n.isNaN => Number(Double.NaN)
    case ENumber(n)            => Number(n)
    case EBigInt(n)            => BigInt(n)
    case EStr(str)             => Str(str)
    case EBool(b)              => Bool(b)
    case EUndef()              => Undef
    case ENull()               => Null
    case EEnum(name)           => Enum(name)
    case ECodeUnit(c)          => CodeUnit(c)
  }

  /** short circuit evaluation */
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value =
    val l = eval(left)
    (bop, l) match
      case (BOp.And, Bool(false)) => Bool(false)
      case (BOp.Or, Bool(true))   => Bool(true)
      case _ =>
        val r = eval(right)
        Interpreter.eval(bop, l, r)

  /** get initial local variables */
  def getLocals(
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
        if (tyCheck && paramTy.isDefined) {
          val thisMethodCall = func.isMethod && params.indexOf(param) == 0
          if (!paramTy.contains(arg, st) && !thisMethodCall) {
            mismatches += TypeMismatch(
              "ParamTypeMismatch",
              func.irFunc.name,
              Some(param.lhs.name),
              getSource(st),
            )
          }
        }
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** transition for references */
  def eval(ref: Ref): RefTarget = ref match
    case x: Var => VarTarget(x)
    case Field(ref, expr) =>
      var base = st(eval(ref))
      val f = eval(expr)
      FieldTarget(base, f)

  /** define call result to state and move to next */
  def setCallResult(x: Var, value: Value): Unit =
    st.define(x, value)
    st.context.moveNode

  /** set return value and move to the exit node */
  def moveBranch(branch: Branch, cond: Boolean): Unit =
    st.context.cursor = Cursor(
      if (cond) branch.thenNode
      else branch.elseNode,
      st.func,
    )

  /** set return value and move to the exit node */
  def moveExit: Unit =
    st.context.cursor = ExitCursor(st.func)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  /** control flow graphs */
  private given cfg: CFG = st.cfg

  /** type model */
  private def tyModel = cfg.tyModel

  /** grammar */
  private def grammar = cfg.grammar

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$EVAL_LOG_DIR/log"))

  /** mismatches while runtime type checking */
  case class TypeMismatch(
    tag: String, // "ParamTypeMismatch" or "ReturnTypeMismatch"
    algo: String,
    param: Option[String], // Defined if tag is "ParamTypeMismatch"
    source: Option[String],
  )
  private val mismatches: MSet[TypeMismatch] = MSet()

  /** get source of the program point */
  private def getSource(st: State): Option[String] = for {
    ast <- st.context.astOpt.orElse(
      st.callStack.view.flatMap(_.context.astOpt).headOption,
    )
    loc <- ast.loc
    filename <- loc.filename
  } yield filename.stripPrefix(CUR_DIR).stripPrefix("/")

  /** cache to get syntax-directed operation (SDO) */
  private val getSdo = cached[(Ast, String), Option[(Ast, Func)]](_.getSdo(_))

  // create a new context
  private def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Context,
  ): Context = createContext(call, func, locals, Some(prevCtxt))

  // create a new context
  private def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Option[Context] = None,
  ): Context = {
    lazy val prevFeatureStack = prevCtxt.fold(Nil)(_.featureStack)
    lazy val prevCallPath = prevCtxt.fold(CallPath())(_.callPath)
    lazy val prevNearest = prevCtxt.flatMap(_.nearest)
    func.head match
      case Some(head: SyntaxDirectedOperationHead) =>
        val feature = SyntacticFeature(func, head)
        val nearest = for {
          case AstValue(ast @ Syntactic(name, _, idx, _)) <- locals.get(
            Name("this"),
          )
          loc <- ast.loc
        } yield Nearest(name, idx, ast.subIdx, loc)
        Context(func, locals, feature :: prevFeatureStack, nearest)
      case Some(head: BuiltinHead) => Context(func, locals)
      case _ =>
        Context(
          func,
          locals,
          prevFeatureStack,
          prevNearest,
          prevCallPath + call,
        )
  }

  // heap inspector
  private def inspect(v: Value, st: State): String = v match
    case addr: Addr => st(addr).toString
    case value      => value.toString
}

/** IR interpreter with a CFG */
object Interpreter {
  def apply(
    st: State,
    log: Boolean = false,
    detail: Boolean = false,
    logPW: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    tyCheck: Boolean = false,
  ): State = new Interpreter(st, log, detail, logPW, timeLimit, tyCheck).result

  /** transition for lexical SDO */
  def eval(lex: Lexical, sdoName: String): Value = {
    import ESValueParser.*
    val Lexical(name, str) = lex
    (name, sdoName) match {
      case (_, "StringValue") if StringValue.of.contains(name) =>
        StringValue.of(name)(str)
      case (_, "NumericValue") if NumericValue.of.contains(name) =>
        NumericValue.of(name)(str)
      case (_, "MV") if MV.of.contains(name) =>
        MV.of(name)(str)
      case (_, "SV") if SV.of.contains(name) =>
        SV.of(name)(str)
      case (_, "TV") if TV.of.contains(name) =>
        TV.of(name)(str)
      case (_, "TRV") if TRV.of.contains(name) =>
        TRV.of(name)(str)
      case ("RegularExpressionLiteral", name) =>
        throw NotSupported(Feature)(List("RegExp"))
      case _ =>
        throw InvalidAstField(lex, Str(sdoName))
    }
  }

  /** transition for unary operators */
  def eval(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      // mathematic values
      case (Abs, m: Math)   => abs(m)
      case (Floor, m: Math) => floor(m)
      // numeric values
      case (Neg, Number(n)) => Number(-n)
      case (Neg, Math(n))   => Math(-n)
      case (Neg, POS_INF)   => NEG_INF
      case (Neg, NEG_INF)   => POS_INF
      case (Neg, BigInt(b)) => BigInt(-b)
      // boolean
      case (Not, Bool(b)) => Bool(!b)
      // bitwise
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, Number(n)) => Number(~(n.toInt))
      case (BNot, BigInt(b)) => BigInt(~b)
      case (_, value)        => throw InvalidUnaryOp(uop, value)

  /** transition for binary operators */
  def eval(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    (bop, left, right) match {
      // double operations
      case (Add, Number(l), Number(r))  => Number(l + r)
      case (Sub, Number(l), Number(r))  => Number(l - r)
      case (Mul, Number(l), Number(r))  => Number(l * r)
      case (Pow, Number(l), Number(r))  => Number(math.pow(l, r))
      case (Div, Number(l), Number(r))  => Number(l / r)
      case (Mod, Number(l), Number(r))  => Number(l % r)
      case (UMod, Number(l), Number(r)) => Number(l %% r)
      case (Lt, Number(l), Number(r)) if (l equals -0.0) && (r equals 0.0) =>
        Bool(true)
      case (Lt, Number(l), Number(r)) => Bool(l < r)

      // mathematical value operations
      case (Add, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r)) => Math(l - r)
      case (Mul, Math(l), Math(r)) => Math(l * r)
      case (Div, Math(l), Math(r)) =>
        // XXX rounded by DECIMAL128 to handle non-terminating decimal
        // expansion. For example, 1 / 3 = 1.3333...
        Math(l(DECIMAL128) / r(DECIMAL128))
      case (Mod, Math(l), Math(r)) =>
        val m = l % r
        Math(if (m * r) < 0 then r + m else m)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) if r.isValidInt && r >= 0 =>
        Math(l.pow(r.toInt))
      case (Pow, Math(l), Math(r)) => Math(math.pow(l.toDouble, r.toDouble))
      // TODO consider 2's complement 32-bit strings
      case (BAnd, Math(l), Math(r)) => Math(l.toLong & r.toLong)
      case (BOr, Math(l), Math(r))  => Math(l.toLong | r.toLong)
      case (BXOr, Math(l), Math(r)) => Math(l.toLong ^ r.toLong)
      case (LShift, Math(l), Math(r)) =>
        Math((l.toInt << r.toInt).toLong)
      case (SRShift, Math(l), Math(r)) =>
        Math((l.toInt >> r.toInt).toLong)
      case (URShift, Math(l), Math(r)) =>
        Math((l.toLong << 32) >>> (32 + (r.toLong % 32)))
      case (Lt, Math(l), Math(r)) => Bool(l < r)

      // extended mathematical value operations
      case (Add, POS_INF, Math(_))          => POS_INF
      case (Add, Math(_), POS_INF)          => POS_INF
      case (Add, POS_INF, POS_INF)          => POS_INF
      case (Add, NEG_INF, Math(_))          => NEG_INF
      case (Add, Math(_), NEG_INF)          => NEG_INF
      case (Add, NEG_INF, NEG_INF)          => NEG_INF
      case (Sub, POS_INF, Math(_))          => POS_INF
      case (Sub, POS_INF, NEG_INF)          => POS_INF
      case (Sub, Math(_), POS_INF)          => NEG_INF
      case (Sub, NEG_INF, Math(_))          => NEG_INF
      case (Sub, NEG_INF, POS_INF)          => NEG_INF
      case (Sub, Math(_), NEG_INF)          => POS_INF
      case (Mul, POS_INF, Math(r)) if r > 0 => POS_INF
      case (Mul, POS_INF, Math(r)) if r < 0 => NEG_INF
      case (Mul, Math(l), POS_INF) if l > 0 => POS_INF
      case (Mul, Math(l), POS_INF) if l < 0 => NEG_INF
      case (Mul, POS_INF, POS_INF)          => POS_INF
      case (Mul, POS_INF, NEG_INF)          => NEG_INF
      case (Mul, NEG_INF, POS_INF)          => NEG_INF
      case (Mul, NEG_INF, Math(r)) if r > 0 => NEG_INF
      case (Mul, NEG_INF, Math(r)) if r < 0 => POS_INF
      case (Mul, Math(l), NEG_INF) if l > 0 => NEG_INF
      case (Mul, Math(l), NEG_INF) if l < 0 => POS_INF
      case (Mul, NEG_INF, NEG_INF)          => POS_INF
      case (Lt, POS_INF, Math(_))           => Bool(false)
      case (Lt, Math(_), POS_INF)           => Bool(true)
      case (Lt, NEG_INF, Math(_))           => Bool(true)
      case (Lt, Math(_), NEG_INF)           => Bool(false)
      case (Lt, NEG_INF, POS_INF)           => Bool(true)
      case (Lt, POS_INF, NEG_INF)           => Bool(false)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Number(l), Number(r))     => Bool(l equals r)
      case (Eq, AstValue(l), AstValue(r)) => Bool(l eq r)
      case (Eq, l, r)                     => Bool(l == r)

      // numeric equality operations
      case (Equal, Math(l), Math(r))         => Bool(l == r)
      case (Equal, Infinity(l), Infinity(r)) => Bool(l == r)
      case (Equal, Number(l), Number(r))     => Bool(l == r)
      case (Equal, BigInt(l), BigInt(r))     => Bool(l == r)
      case (Equal, POS_INF, Math(_))         => Bool(false)
      case (Equal, Math(_), POS_INF)         => Bool(false)
      case (Equal, NEG_INF, Math(_))         => Bool(false)
      case (Equal, Math(_), NEG_INF)         => Bool(false)

      // big integers
      case (Add, BigInt(l), BigInt(r))     => BigInt(l + r)
      case (LShift, BigInt(l), BigInt(r))  => BigInt(l << r.toInt)
      case (SRShift, BigInt(l), BigInt(r)) => BigInt(l >> r.toInt)
      case (Sub, BigInt(l), BigInt(r))     => BigInt(l - r)
      case (Mul, BigInt(l), BigInt(r))     => BigInt(l * r)
      case (Div, BigInt(l), BigInt(r))     => BigInt(l / r)
      case (Mod, BigInt(l), BigInt(r))     => BigInt(l % r)
      case (UMod, BigInt(l), BigInt(r))    => BigInt(l %% r)
      case (Lt, BigInt(l), BigInt(r))      => Bool(l < r)
      case (BAnd, BigInt(l), BigInt(r))    => BigInt(l & r)
      case (BOr, BigInt(l), BigInt(r))     => BigInt(l | r)
      case (BXOr, BigInt(l), BigInt(r))    => BigInt(l ^ r)
      case (Pow, BigInt(l), BigInt(r))     => BigInt(l.pow(r.toInt))

      case (_, lval, rval) => throw InvalidBinaryOp(bop, lval, rval)
    }

  /** transition for variadic operators */
  def eval(vop: VOp, vs: List[Value]): Value =
    import VOp.*
    if (vs.isEmpty) throw InvalidVariadicOp(vop)
    vop match
      case Min =>
        if (vs.contains(NEG_INF)) NEG_INF
        else {
          val filtered = vs.filter(_ != POS_INF)
          if (filtered.isEmpty) POS_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
      case Max =>
        if (vs.contains(POS_INF)) POS_INF
        else {
          val filtered = vs.filter(_ != NEG_INF)
          if (filtered.isEmpty) NEG_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
        vopEval(_.asMath, _ max _, Math(_), vs)
      case Concat =>
        def toString(v: Value): String = v match
          case Str(s)      => s
          case CodeUnit(c) => c.toString
          case v           => throw NoString(v)
        vopEval(toString, _ + _, Str(_), vs)

  /** transition for mathematical operators */
  def eval(mop: MOp, st: State, vs: List[Value]): Value =
    import math.*
    (mop, vs) match
      case (MOp.Expm1, List(Math(x))) => Math(expm1(x.toDouble))
      case (MOp.Log10, List(Math(x))) => Math(log10(x.toDouble))
      case (MOp.Log2, List(Math(x)))  => Math(log(x.toDouble) / log(2))
      case (MOp.Cos, List(Math(x)))   => Math(cos(x.toDouble))
      case (MOp.Cbrt, List(Math(x)))  => Math(cbrt(x.toDouble))
      case (MOp.Exp, List(Math(x)))   => Math(exp(x.toDouble))
      case (MOp.Cosh, List(Math(x)))  => Math(cosh(x.toDouble))
      case (MOp.Sinh, List(Math(x)))  => Math(sinh(x.toDouble))
      case (MOp.Tanh, List(Math(x)))  => Math(tanh(x.toDouble))
      case (MOp.Acos, List(Math(x)))  => Math(acos(x.toDouble))
      case (MOp.Acosh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("acosh")
      case (MOp.Asinh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("asinh")
      case (MOp.Atanh, List(Math(x))) =>
        throw NotSupported(Metalanguage)("atanh")
      case (MOp.Asin, List(Math(x))) => Math(asin(x.toDouble))
      case (MOp.Atan2, List(Math(x), Math(y))) =>
        Math(atan2(x.toDouble, y.toDouble))
      case (MOp.Atan, List(Math(x)))  => Math(atan(x.toDouble))
      case (MOp.Log1p, List(Math(x))) => Math(log1p(x.toDouble))
      case (MOp.Log, List(Math(x)))   => Math(log(x.toDouble))
      case (MOp.Sin, List(Math(x)))   => Math(sin(x.toDouble))
      case (MOp.Sqrt, List(Math(x)))  => Math(sqrt(x.toDouble))
      case (MOp.Tan, List(Math(x)))   => Math(tan(x.toDouble))
      case _                          => throw InvalidMathOp(mop, vs)

  /** the absolute value operation for mathematical values */
  def abs(m: Math): Math = Math(m.decimal.abs)

  /** the floor operation for mathematical values */
  def floor(m: Math): Math =
    val Math(d) = m
    if (d.isWhole) m
    else Math(d - (d % 1) - (if (d < 0) 1 else 0))

  /** helpers for make transition for variadic operators */
  def vopEval[T](
    f: Value => T,
    op: (T, T) => T,
    g: T => Value,
    vs: List[Value],
  ) = g(vs.map(f).reduce(op))
}
