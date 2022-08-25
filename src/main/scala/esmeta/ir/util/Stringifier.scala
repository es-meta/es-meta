package esmeta.ir.util

import esmeta.LINE_SEP
import esmeta.ir.*
import esmeta.lang.Syntax
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.ListBuffer

/** stringifier for IR */
class Stringifier(detail: Boolean, location: Boolean) {
  // elements
  given elemRule: Rule[IRElem] = (app, elem) =>
    elem match {
      case elem: Program    => programRule(app, elem)
      case elem: Func       => funcRule(app, elem)
      case elem: Func.Kind  => funcKindRule(app, elem)
      case elem: Func.Param => paramRule(app, elem)
      case elem: Inst       => instRule(app, elem)
      case elem: Expr       => exprRule(app, elem)
      case elem: UOp        => uopRule(app, elem)
      case elem: BOp        => bopRule(app, elem)
      case elem: VOp        => vopRule(app, elem)
      case elem: COp        => copRule(app, elem)
      case elem: Ref        => refRule(app, elem)
      case elem: Type       => tyRule(app, elem)
    }

  // programs
  given programRule: Rule[Program] = (app, program) =>
    given Rule[Iterable[Func]] = iterableRule(sep = LINE_SEP)
    app >> program.funcs

  // functions
  given funcRule: Rule[Func] = (app, func) =>
    val Func(main, kind, name, params, retTy, body, _) = func
    given Rule[Iterable[Func.Param]] = iterableRule("(", ", ", ")")
    app >> (if (main) "@main " else "") >> "def " >> kind
    app >> name >> params >> ": " >> retTy >> " " >> body

  // function kinds
  given funcKindRule: Rule[Func.Kind] = (app, kind) =>
    import Func.Kind.*
    app >> (kind match {
      case AbsOp        => ""
      case NumMeth      => "<NUM>:"
      case SynDirOp     => "<SYNTAX>:"
      case ConcMeth     => "<CONC>:"
      case InternalMeth => "<INTERNAL>:"
      case Builtin      => "<BUILTIN>:"
      case Clo          => "<CLO>:"
      case Cont         => "<CONT>:"
      case BuiltinClo   => "<BUILTIN-CLO>:"
    })

  // function parameters
  given paramRule: Rule[Func.Param] = (app, param) =>
    val Func.Param(name, optional, ty) = param
    app >> name >> (if (optional) "?" else "") >> ": " >> ty

  // instructions
  given instRule: Rule[Inst] = withLoc { (app, inst) =>
    inst match
      case IExpr(expr) =>
        app >> expr
      case ILet(lhs, expr) =>
        app >> "let " >> lhs >> " = " >> expr
      case IAssign(ref, expr) =>
        app >> ref >> " = " >> expr
      case IDelete(ref) =>
        app >> "delete " >> ref
      case IPush(from, to, front) =>
        app >> "push "
        if (front) app >> from >> " > " >> to
        else app >> to >> " < " >> from
      case IRemoveElem(list, elem) =>
        app >> "remove-elem " >> list >> " " >> elem
      case IReturn(expr) =>
        app >> "return " >> expr
      case IAssert(expr) =>
        app >> "assert " >> expr
      case IPrint(expr) =>
        app >> "print " >> expr
      case INop() =>
        app >> "nop"
      case ISeq(insts) =>
        if (insts.isEmpty) app >> "{}"
        else app.wrap(for { i <- insts } app :> i)
      case IIf(cond, thenInst, elseInst) =>
        app >> "if " >> cond >> " " >> thenInst
        app >> " else " >> elseInst
      case ILoop(kind, cond, body) =>
        app >> "loop[" >> kind >> "] " >> cond >> " " >> body
      case ICall(lhs, fexpr, args) =>
        given Rule[List[Expr]] = iterableRule("(", ", ", ")")
        app >> "call " >> lhs >> " = " >> fexpr >> args
      case IMethodCall(lhs, base, method, args) =>
        given Rule[List[Expr]] = iterableRule("(", ", ", ")")
        app >> "method-call " >> lhs >> " = "
        app >> base >> "->" >> method >> args
      case ISdoCall(lhs, ast, method, args) =>
        given Rule[List[Expr]] = iterableRule("(", ", ", ")")
        app >> "sdo-call " >> lhs >> " = "
        app >> ast >> "->" >> method >> args
  }

  // expressions
  given exprRule: Rule[Expr] = (app, expr) =>
    expr match
      case EComp(tyExpr, valExpr, tgtExpr) =>
        app >> "comp[" >> tyExpr >> "/" >> tgtExpr >> "](" >> valExpr >> ")"
      case EIsCompletion(expr) =>
        app >> "(comp? " >> expr >> ")"
      case EReturnIfAbrupt(expr, check) =>
        app >> "[" >> (if (check) "?" else "!") >> " " >> expr >> "]"
      case EPop(list, front) =>
        app >> "(pop " >> (if (front) "<" else ">") >> " " >> list >> ")"
      case EParse(code, rule) =>
        app >> "(parse " >> code >> " " >> rule >> ")"
      case EGrammar(name, params) =>
        app >> "(grammar |" >> name >> "|"
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule("[", "", "]")
        app >> params >> ")"
      case ESourceText(expr) =>
        app >> "(source-text " >> expr >> ")"
      case EYet(msg) =>
        app >> "(yet \"" >> normStr(msg) >> "\")"
      case EContains(list, elem, field) =>
        app >> "(contains " >> list >> " " >> elem
        field.map { case (t, f) => app >> ": " >> t >> " " >> f }
        app >> ")"
      case ESubstring(expr, from, to) => to match {
        case Some(to) => app >> "(substring " >> expr >> " " >> from >> " " >> to >> ")"
        case None => app >> "(substring " >> expr >> " " >> from >> ")"
      }
      case ERef(ref) =>
        app >> ref
      case EUnary(uop, expr) =>
        app >> "(" >> uop >> " " >> expr >> ")"
      case EBinary(bop, left, right) =>
        app >> "(" >> bop >> " " >> left >> " " >> right >> ")"
      case EClamp(target, lower, upper) =>
        app >> "(clamp " >> target >> " " >> lower >> " " >> upper >> ")"
      case EVariadic(vop, exprs) =>
        given Rule[Iterable[Expr]] = iterableRule(sep = " ")
        app >> "(" >> vop >> " " >> exprs >> ")"
      case EConvert(cop, expr) =>
        app >> "(" >> cop >> " " >> expr >> ")"
      case ETypeOf(base) =>
        app >> "(typeof " >> base >> ")"
      case ETypeCheck(expr, ty) =>
        app >> "(? " >> expr >> ": " >> ty >> ")"
      case EDuplicated(expr) =>
        app >> "(duplicated " >> expr >> ")"
      case EIsArrayIndex(expr) =>
        app >> "(array-index " >> expr >> ")"
      case EClo(fname, captured) =>
        given Rule[Iterable[Name]] = iterableRule("[", ", ", "]")
        app >> "clo<" >> fname
        if (!captured.isEmpty) app >> ", " >> captured
        app >> ">"
      case ECont(fname) =>
        app >> "cont<" >> fname >> ">"
      case expr: AstExpr =>
        astExprRule(app, expr)
      case expr: AllocExpr =>
        allocExprRule(app, expr)
      case expr: LiteralExpr =>
        literalExprRule(app, expr)

  // abstract syntax tree (AST) expressions
  lazy val astExprRule: Rule[AstExpr] = (app, ast) =>
    ast match {
      case ESyntactic(name, args, rhsIdx, children) =>
        app >> "|" >> name >> "|"
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule("[", "", "]")
        if (!args.isEmpty) app >> args
        app >> "<" >> rhsIdx >> ">"
        given eo: Rule[Option[Expr]] = optionRule("")
        given el: Rule[List[Option[Expr]]] = iterableRule("(", ", ", ")")
        if (!children.isEmpty) app >> children
        app
      case ELexical(name, expr) =>
        app >> "|" >> name >> "|(" >> expr >> ")"
    }

  // allocation expressions
  lazy val allocExprRule: Rule[AllocExpr] = (app, expr) =>
    expr match {
      case EMap(tname, fields) =>
        given Rule[Iterable[(Expr, Expr)]] = iterableRule("(", ", ", ")")
        app >> "(new " >> tname >> fields >> ")"
      case EList(exprs) =>
        given Rule[Iterable[Expr]] = iterableRule("[", ", ", "]")
        app >> "(new " >> exprs >> ")"
      case EListConcat(exprs) =>
        given Rule[Iterable[Expr]] = iterableRule(sep = " ")
        app >> "(list-concat " >> exprs >> ")"
      case ESymbol(desc) =>
        app >> "(new '" >> desc >> ")"
      case ECopy(obj) =>
        app >> "(copy " >> obj >> ")"
      case EKeys(map, intSorted) =>
        app >> "(keys" >> (if (intSorted) "-int" else "") >> " "
        app >> map >> ")"
      case EGetChildren(kindOpt, ast) =>
        app >> "(get-children "
        kindOpt.foreach(kind => app >> kind >> " ")
        app >> ast >> ")"
    }
    if (expr.asite == -1) app
    else app >> "[#" >> expr.asite >> "]"

  // literals
  lazy val literalExprRule: Rule[LiteralExpr] = (app, lit) =>
    lit match {
      case EMathVal(n)                      => app >> n
      case ENumber(Double.PositiveInfinity) => app >> "+INF"
      case ENumber(Double.NegativeInfinity) => app >> "-INF"
      case ENumber(n) if n.isNaN            => app >> "NaN"
      case ENumber(n)                       => app >> n >> "f"
      case EBigInt(n)                       => app >> n >> "n"
      case EStr(str)    => app >> "\"" >> normStr(str) >> "\""
      case EBool(b)     => app >> b
      case EUndef       => app >> "undefined"
      case ENull        => app >> "null"
      case EAbsent      => app >> "absent"
      case EConst(name) => app >> "~" >> name >> "~"
      case ECodeUnit(c) => app >> c.toInt >> "cu"
    }

  // unary operators
  given uopRule: Rule[UOp] = (app, uop) =>
    import UOp.*
    app >> (uop match {
      case Abs   => "abs"
      case Floor => "floor"
      case Neg   => "-"
      case Not   => "!"
      case BNot  => "~"
    })

  // binary operators
  given bopRule: Rule[BOp] = (app, bop) =>
    import BOp.*
    app >> (bop match
      case Plus    => "+"
      case Sub     => "-"
      case Mul     => "*"
      case Pow     => "**"
      case Div     => "/"
      case UMod    => "%%"
      case Mod     => "%"
      case Eq      => "="
      case Equal   => "=="
      case And     => "&&"
      case Or      => "||"
      case Xor     => "^^"
      case BAnd    => "&"
      case BOr     => "|"
      case BXOr    => "^"
      case LShift  => "<<"
      case Lt      => "<"
      case URShift => ">>>"
      case SRShift => ">>"
    )

  // variadic operators
  given vopRule: Rule[VOp] = (app, vop) =>
    import VOp.*
    app >> (vop match
      case Min    => "min"
      case Max    => "max"
      case Concat => "concat"
    )

  // conversion operators
  given copRule: Rule[COp] = (app, cop) =>
    import COp.*
    cop match {
      case ToBigInt => app >> "[bigint]"
      case ToNumber => app >> "[number]"
      case ToMath   => app >> "[math]"
      case ToStr(radix) =>
        app >> "[str"
        radix.map(app >> " " >> _)
        app >> "]"
    }

  // references
  lazy val inlineProp = "([_a-zA-Z][_a-zA-Z0-9]*)".r
  given refRule: Rule[Ref] = (app, ref) =>
    ref match {
      case Prop(ref, EStr(inlineProp(str))) => app >> ref >> "." >> str
      case Prop(ref, expr)                  => app >> ref >> "[" >> expr >> "]"
      case id: Id                           => idRule(app, id)
    }

  // identifiers
  given idRule: Rule[Id] = (app, id) =>
    id match {
      case Global(name) => app >> "@" >> name
      case Name(name)   => app >> name
      case Temp(id)     => app >> "%" >> id
    }

  // types
  given tyRule: Rule[Type] = (app, ty) => app >> ty.name

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // append locations
  private def withLoc(rule: Rule[Inst]): Rule[Inst] = (app, elem) =>
    given Rule[Option[Syntax]] = (app, langOpt) =>
      for {
        lang <- langOpt
        loc <- lang.loc
      } app >> " @ " >> loc.toString
      app
    rule(app, elem)
    if (location) app >> elem.langOpt else app
}
