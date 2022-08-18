package esmeta.lang.util

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.lang.*

/** stringifier for metalanguage */
class Stringifier(detail: Boolean, location: Boolean) {
  // elements
  given elemRule: Rule[LangElem] = (app, elem) =>
    elem match {
      case elem: Syntax                => syntaxRule(app, elem)
      case elem: PredicateCondition.Op => predCondOpRule(app, elem)
      case elem: MathOpExpression.Op   => mathOpExprOpRule(app, elem)
      case elem: BinaryExpression.Op   => binExprOpRule(app, elem)
      case elem: UnaryExpression.Op    => unExprOpRule(app, elem)
      case elem: XRefExpression.Op     => xrefExprOpRule(app, elem)
      case elem: BinaryCondition.Op    => binCondOpRule(app, elem)
      case elem: CompoundCondition.Op  => compCondOpRule(app, elem)
    }

  // syntax
  given syntaxRule: Rule[Syntax] = (app, syn) =>
    syn match {
      case syn: Block      => blockRule(app, syn)
      case syn: Step       => stepRule(app, syn)
      case syn: SubStep    => subStepRule(app, syn)
      case syn: Expression => exprRule(app, syn)
      case syn: Condition  => condRule(app, syn)
      case syn: Reference  => refRule(app, syn)
      case syn: Type       => typeRule(app, syn)
      case syn: Property   => propRule(app, syn)
      case syn: Intrinsic  => intrRule(app, syn)
    }

  // blocks
  given blockRule: Rule[Block] = (app, block) =>
    given Rule[Step] = stepWithUpperRule(true)
    if (detail) app.wrap("", "")(block match {
      case StepBlock(steps) =>
        steps.foreach(app :> "1. " >> _)
      case ExprBlock(exprs) =>
        exprs.foreach(app :> "* " >> _)
      case Figure(lines) =>
        app :> "<figure>"
        app.wrap("", "")(lines.map(app :> _))
        app :> "</figure>"
    })
    else app >> " [...]"

  // sub-steps
  given subStepRule: Rule[SubStep] = (app, subStep) =>
    given Rule[Step] = stepWithUpperRule(true)
    val SubStep(idTag, step) = subStep
    idTag.map(app >> "[id=\"" >> _ >> "\"] ")
    app >> step

  // steps
  given stepRule: Rule[Step] = stepWithUpperRule(false)
  def stepWithUpperRule(upper: Boolean): Rule[Step] = withLoc { (app, step) =>
    given Rule[First] = firstRule(upper)
    step match {
      case LetStep(x, expr) =>
        given Rule[Expression] = endWithExprRule
        app >> First("let ") >> x >> " be " >> expr
      case SetStep(x, expr) =>
        given Rule[Expression] = endWithExprRule
        app >> First("set ") >> x >> " to " >> expr
      case IfStep(cond, thenStep, elseStep) =>
        app >> First("if ") >> cond >> ", "
        if (thenStep.isInstanceOf[BlockStep]) app >> "then"
        app >> thenStep
        elseStep match {
          case Some(ifStep: IfStep)  => app :> "1. Else " >> ifStep
          case Some(step: BlockStep) => app :> "1. Else," >> step
          case Some(step)            => app :> "1. Else, " >> step
          case None                  => app
        }
      case ReturnStep(expr) =>
        given Rule[Expression] = endWithExprRule
        app >> First("return")
        expr match {
          case None    => app >> "."
          case Some(e) => app >> " " >> e
        }
      case AssertStep(cond) =>
        app >> First("assert: ") >> cond >> "."
      case ForEachStep(ty, elem, expr, ascending, body) =>
        app >> First("for each ")
        ty.map(app >> _ >> " ")
        app >> elem >> " of " >> expr >> ", "
        if (!ascending) app >> "in reverse List order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachIntegerStep(x, start, cond, ascending, body) =>
        app >> First("for each integer ") >> x >> " starting with " >> start
        app >> " such that " >> cond >> ", in "
        app >> (if (ascending) "ascending" else "descending") >> " order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachArrayIndexStep(key, array, start, ascending, body) =>
        app >> First("for each own property key ") >> key >> " of " >> array
        app >> " that is an array index, whose numeric value is greater than or equal to "
        app >> start >> ", " >> "in descending numeric index order, "
        if (body.isInstanceOf[BlockStep]) app >> "do"
        app >> body
      case ForEachParseNodeStep(x, expr, body) =>
        app >> First("for each child node ") >> x
        app >> " of " >> expr >> ", do" >> body
      case ThrowStep(expr) =>
        app >> First("throw ") >> expr >> "."
      case PerformStep(expr) =>
        app >> First("perform ") >> expr >> "."
      case PerformBlockStep(block) =>
        app >> First("perform ")
        app >> "the following substeps in an implementation-defined order:"
        app >> block
      case AppendStep(expr, ref) =>
        app >> First("append ") >> expr >> " to " >> ref >> "."
      case PrependStep(expr, ref) =>
        app >> First("prepend ") >> expr >> " to " >> ref >> "."
      case RepeatStep(cond, body) =>
        app >> First("repeat, ")
        for { c <- cond } app >> "while " >> c >> ","
        app >> body
      case PushCtxtStep(ref) =>
        app >> First("push ") >> ref >> " onto the execution context stack;"
        app >> " " >> ref >> " is now the running execution context."
      case NoteStep(note) =>
        app >> "NOTE: " >> note
      case SuspendStep(context, remove) =>
        app >> First("suspend ") >> context
        if (remove) app >> " and remove it from the execution context stack"
        app >> "."
      case SetEvaluationStateStep(context, param, step) =>
        app >> First("set the code evaluation state of ") >> context >> " "
        app >> "such that when evaluation is resumed "
        param match
          case None    => app >> "for that execution context "
          case Some(p) => app >> "with a " >> p >> " "
        app >> "the following steps will be performed:" >> step
      case ResumeEvaluationStep(context, argOpt, paramOpt, body) =>
        given Rule[Step] = stepWithUpperRule(true)
        app >> """<emu-meta effects="user-code">Resume the suspended evaluation of """ >> context
        app >> "</emu-meta>"
        for { arg <- argOpt }
          app >> " using " >> arg >> " as the result of the operation that suspended it"
        app >> "."
        for { param <- paramOpt }
        app >> " Let " >> param >> " be the value returned by the resumed computation."
        for { step <- body } app :> "1. " >> step // TODO
        app
      case ReturnToResumeStep(context, retStep) =>
        given Rule[Step] = stepWithUpperRule(true)
        app >> retStep
        app :> "1. NOTE: This returns to the evaluation of the operation "
        app >> "that had most previously resumed evaluation of " >> context >> "."
      case BlockStep(block) =>
        app >> block
      case YetStep(expr) =>
        app >> expr
    }
  }
  private case class First(str: String)
  private def firstRule(upper: Boolean): Rule[First] = (app, first) => {
    val First(str) = first
    app >> (if (upper) str.head.toUpper else str.head) >> str.substring(1)
  }
  private def endWithExprRule: Rule[Expression] = (app, expr) => {
    expr match {
      case multi: MultilineExpression => app >> expr
      case _                          => app >> expr >> "."
    }
  }

  // expressions
  given exprRule: Rule[Expression] = withLoc { (app, expr) =>
    expr match {
      case StringConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the string-concatenation of " >> exprs
      case ListConcatExpression(exprs) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        app >> "the list-concatenation of " >> exprs
      case RecordExpression(ty, fields) =>
        given Rule[(FieldLiteral, Expression)] = {
          case (app, (field, expr)) =>
            app >> field >> ": " >> expr
        }
        given Rule[List[(FieldLiteral, Expression)]] =
          iterableRule("{ ", ", ", " }")
        app >> ty >> " "
        if (fields.isEmpty) app >> "{ }"
        else app >> fields
      case LengthExpression(expr) =>
        app >> "the length of " >> expr
      case SubstringExpression(expr, from, to) =>
        app >> "the substring of " >> expr >> " from " >> from >> " to " >> to
      case NumberOfExpression(expr) =>
        app >> "the number of elements in " >> expr
      case SourceTextExpression(expr) =>
        app >> "the source text matched by " >> expr
      case CoveredByExpression(code, rule) =>
        app >> "the " >> rule >> " that is covered by " >> code
      case GetChildrenExpression(nt, expr) =>
        app >> "the List of " >> nt >> " items in " >> expr >> ", in source text order"
      case IntrinsicExpression(intr) =>
        app >> intr
      case XRefExpression(kind, id) =>
        app >> kind >> " <emu-xref href=\"#" >> id >> "\"></emu-xref>"
      case expr: CalcExpression =>
        calcExprRule(app, expr)
      case BitwiseExpression(left, op, right) =>
        app >> "the result of applying the " >> op >> " to " >> left >> " and " >> right
      case expr: InvokeExpression =>
        invokeExprRule(app, expr)
      case ListExpression(Nil) => app >> "« »"
      case ListExpression(entries) =>
        given Rule[Iterable[Expression]] = iterableRule("« ", ", ", " »")
        app >> entries
      case SoleElementExpression(expr) =>
        app >> "the sole element of " >> expr
      case YetExpression(str, block) =>
        app >> str
        block.fold(app)(app >> _)
      case multi: MultilineExpression => app >> multi
    }
  }

  // multiline expressions
  given multilineExprRule: Rule[MultilineExpression] = (app, expr) =>
    expr match {
      case AbstractClosureExpression(params, captured, body) =>
        given Rule[List[Variable]] = listNamedSepRule(namedSep = "and")
        app >> "a new Abstract Closure with"
        if (params.isEmpty) app >> " no parameters "
        else {
          given Rule[List[Variable]] = iterableRule(sep = ", ")
          app >> " parameters (" >> params >> ") "
        }
        app >> "that captures " >> captured
        app >> " and performs the following steps when called:" >> body
    }

  // calculation expressions
  given calcExprRule: Rule[CalcExpression] = calcExprRuleWithLevel(0)

  def calcExprRuleWithLevel(level: Int): Rule[CalcExpression] = (app, expr) =>
    given Rule[CalcExpression] = calcExprRuleWithLevel(expr.level)
    if (expr.level < level) app >> "("
    expr match {
      case ReturnIfAbruptExpression(expr, check) =>
        app >> (if (check) "?" else "!") >> " " >> expr
      case ReferenceExpression(ref) =>
        app >> ref
      case MathOpExpression(op, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> op >> args
      case ExponentiationExpression(base, power) =>
        app >> base >> "<sup>" >> power >> "</sup>"
      case BinaryExpression(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case UnaryExpression(op, expr) =>
        app >> op >> expr
      case lit: Literal =>
        litRule(app, lit)
    }
    if (expr.level < level) app >> ")"
    app

  // operators for mathematical operation expressions
  given mathOpExprOpRule: Rule[MathOpExpression.Op] = (app, op) =>
    import MathOpExpression.Op.*
    app >> (op match {
      case Max      => "max"
      case Min      => "min"
      case Abs      => "abs"
      case Floor    => "floor"
      case ToBigInt => "ℤ"
      case ToNumber => "𝔽"
      case ToMath   => "ℝ"
    })

  // operators for binary expressions
  given binExprOpRule: Rule[BinaryExpression.Op] = (app, op) =>
    import BinaryExpression.Op.*
    app >> (op match {
      case Add => "+"
      case Sub => "-"
      case Mul => "×"
      case Div => "/"
      case Mod => "modulo"
    })

  // operators for unary expressions
  given unExprOpRule: Rule[UnaryExpression.Op] = (app, op) =>
    import UnaryExpression.Op.*
    app >> (op match {
      case Neg => "-"
    })

  // operators for emu-xref expressions
  given xrefExprOpRule: Rule[XRefExpression.Op] = (app, op) =>
    import XRefExpression.Op.*
    app >> (op match {
      case Algo          => "the definition specified in"
      case InternalSlots => "the internal slots listed in"
      case ParamLength =>
        "the number of non-optional parameters of the function definition in"
    })

  // literals
  given litRule: Rule[Literal] = (app, lit) =>
    lit match {
      case _: ThisLiteral      => app >> "*this* value"
      case _: NewTargetLiteral => app >> "NewTarget"
      case HexLiteral(hex, name) =>
        app >> f"0x$hex%04x"
        name.map(app >> " (" >> _ >> ")")
        app
      case CodeLiteral(code) => app >> "`" >> code >> "`"
      case NonterminalLiteral(ordinal, name, flags) =>
        given Rule[Iterable[String]] = iterableRule("[", ", ", "]")
        ordinal.map(ordinal => app >> "the " >> ordinal.toOrdinal >> " ")
        app >> "|" >> name
        if (!flags.isEmpty) app >> flags
        app >> "|"
      case ConstLiteral(name) => app >> "~" >> name >> "~"
      case StringLiteral(str) =>
        val replaced = str
          .replace("\\", "\\\\")
          .replace("*", "\\*")
        app >> "*\"" >> replaced >> "\"*"
      case FieldLiteral(name) => app >> "[[" >> name >> "]]"
      case SymbolLiteral(sym) => app >> "@@" >> sym
      case ProductionLiteral(lhs, rhs) =>
        app >> "<emu-grammar>" >> lhs >> " : " >> rhs >> "</emu-grammar>"
      case ErrorObjectLiteral(name) =>
        app >> "a newly created *" >> name >> "* object"
      case _: PositiveInfinityMathValueLiteral => app >> "+∞"
      case _: NegativeInfinityMathValueLiteral => app >> "-∞"
      case DecimalMathValueLiteral(n)          => app >> n
      case NumberLiteral(n) =>
        if (n.isNaN) app >> "*NaN*"
        else
          app >> "*" >> (
            if (n.isPosInfinity) "+∞"
            else if (n.isNegInfinity) "-∞"
            else if (n == 0) if ((1 / n).isPosInfinity) "+0" else "-0"
            else if (n.toInt == n) n.toInt.toString
            else n.toString
          ) >> "*<sub>𝔽</sub>"
      case BigIntLiteral(n)        => app >> "*" >> n >> "*<sub>ℤ</sub>"
      case _: TrueLiteral          => app >> "*true*"
      case _: FalseLiteral         => app >> "*false*"
      case _: UndefinedLiteral     => app >> "*undefined*"
      case _: NullLiteral          => app >> "*null*"
      case _: AbsentLiteral        => app >> "absent"
      case _: UndefinedTypeLiteral => app >> "Undefined"
      case _: NullTypeLiteral      => app >> "Null"
      case _: BooleanTypeLiteral   => app >> "Boolean"
      case _: StringTypeLiteral    => app >> "String"
      case _: SymbolTypeLiteral    => app >> "Symbol"
      case _: NumberTypeLiteral    => app >> "Number"
      case _: BigIntTypeLiteral    => app >> "BigInt"
      case _: ObjectTypeLiteral    => app >> "Object"
    }

  // operators for bitwise expressions
  given bitExprOpRule: Rule[BitwiseExpression.Op] = (app, op) =>
    import BitwiseExpression.Op.*
    app >> (op match {
      case BAnd => "bitwise AND operation"
      case BXOr => "bitwise exclusive OR (XOR) operation"
      case BOr  => "bitwise inclusive OR operation"
    })

  // metalanguage invocation expressions
  given invokeExprRule: Rule[InvokeExpression] = (app, expr) =>
    expr match {
      case InvokeAbstractOperationExpression(name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> name >> args
      case InvokeNumericMethodExpression(ty, name, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> ty >> "::" >> name >> args
      case InvokeAbstractClosureExpression(x, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> x >> args
      case InvokeMethodExpression(base, args) =>
        given Rule[Iterable[Expression]] = iterableRule("(", ", ", ")")
        app >> base >> args
      case InvokeSyntaxDirectedOperationExpression(base, name, args) =>
        given Rule[List[Expression]] = listNamedSepRule(namedSep = "and")
        // handle Evaluation
        if (name == "Evaluation") app >> "the result of evaluating " >> base
        // XXX handle Conatins, Contains always takes one argument
        else if (name == "Contains") app >> base >> " Contains " >> args.head
        // Otherwise
        else {
          app >> name >> " of " >> base
          if (!args.isEmpty) {
            app >> " with argument" >> (if (args.length > 1) "s " else " ")
            app >> args
          }
        }
        app
    }

  // conditions
  given condRule: Rule[Condition] = withLoc { (app, cond) =>
    cond match {
      case ExpressionCondition(expr) =>
        app >> expr
      case InstanceOfCondition(expr, neg, ty) =>
        app >> expr
        // TODO use a/an based on the types
        given Rule[Type] = (app, ty) => app >> "a " >> ty.name
        ty match {
          case t :: Nil => app >> isStr(neg) >> t
          case _ =>
            app >> " is "
            if (neg) {
              given Rule[List[Type]] = listNamedSepRule(namedSep = "nor")
              app >> "neither " >> ty
            } else {
              given Rule[List[Type]] = listNamedSepRule(namedSep = "or")
              app >> "either " >> ty
            }
        }
      case HasFieldCondition(ref, neg, field) =>
        app >> ref >> hasStr(neg)
        // TODO use a/an based on the fields
        app >> "a"
        app >> " " >> field >> " internal slot"
      case ProductionCondition(nt, lhs, rhs) =>
        app >> nt >> " is " >> "<emu-grammar>"
        app >> lhs >> " : " >> rhs
        app >> "</emu-grammar>"
      case PredicateCondition(x, neg, op) =>
        // TODO is/has
        app >> x >> isStr(neg) >> op
      case IsAreCondition(ls, neg, rs) =>
        val single = ls.length == 1
        if (single) app >> ls.head
        else {
          given Rule[List[Expression]] = listNamedSepRule("both ", "and")
          app >> ls
        }
        rs match {
          case r :: Nil => app >> isStr(neg, single) >> r
          case _ =>
            app >> isStr(false, single)
            if (neg) {
              given Rule[List[Expression]] =
                listNamedSepRule(namedSep = "nor")
              app >> "neither " >> rs
            } else {
              given Rule[List[Expression]] =
                listNamedSepRule(namedSep = "or")
              app >> "either " >> rs
            }
        }
      case BinaryCondition(left, op, right) =>
        app >> left >> " " >> op >> " " >> right
      case ContainsWhoseCondition(list, ty, fieldName, expr) =>
        app >> list >> " contains a " >> ty
        app >> " whose [[" >> fieldName >> "]] is " >> expr
      case CompoundCondition(left, op, right) =>
        op match {
          case CompoundCondition.Op.Imply =>
            // TODO handle upper case of `if`
            app >> "If " >> left >> ", then " >> right
          case _ => app >> left >> " " >> op >> " " >> right
        }
    }
  }

  // operators for predicate conditions
  given predCondOpRule: Rule[PredicateCondition.Op] = (app, op) =>
    import PredicateCondition.Op.*
    app >> (op match {
      case Finite           => "finite"
      case Abrupt           => "an abrupt completion"
      case NeverAbrupt      => "never an abrupt completion"
      case Normal           => "a normal completion"
      case Duplicated       => "duplicate entries"
      case Present          => "present"
      case Empty            => "empty"
      case StrictMode       => "strict mode code"
      case ArrayIndex       => "an array index"
      case NonNegative      => "a non-negative integral Number"
      case FalseToken       => "the token `false`"
      case TrueToken        => "the token `true`"
      case DataProperty     => "a data property"
      case AccessorProperty => "an accessor property"
      case FullyPopulated   => "a fully populated Property Descriptor"
      case Nonterminal      => "an instance of a nonterminal"
      case IntegralNumber   => "an integral Number"
    })

  // operators for binary conditions
  given binCondOpRule: Rule[BinaryCondition.Op] = (app, op) =>
    import BinaryCondition.Op.*
    app >> (op match {
      case Eq               => "="
      case NEq              => "≠"
      case LessThan         => "<"
      case LessThanEqual    => "≤"
      case GreaterThan      => ">"
      case GreaterThanEqual => "≥"
      case SameCodeUnits    => "is the same sequence of code units as"
      case Contains         => "contains"
      case NContains        => "does not contain"
    })

  // operators for compound conditions
  given compCondOpRule: Rule[CompoundCondition.Op] = (app, op) => {
    import CompoundCondition.Op.*
    app >> (op match {
      case And   => "and"
      case Or    => "or"
      case Imply => "then" // XXX not used
    })
  }

  // references
  given refRule: Rule[Reference] = withLoc { (app, ref) =>
    ref match {
      case Variable(name: String) =>
        app >> s"_${name}_"
      case _: CurrentRealmRecord =>
        app >> "the current Realm Record"
      case _: ActiveFunctionObject =>
        app >> "the active function object"
      case _: RunningExecutionContext =>
        app >> "the running execution context"
      case _: SecondExecutionContext =>
        app >> "the second to top element of the execution context stack"
      case PropertyReference(base, nt: NonterminalProperty) =>
        app >> nt >> " " >> base
      case PropertyReference(base, prop) =>
        app >> base >> prop
    }
  }

  // properties
  given propRule: Rule[Property] = (app, prop) =>
    prop match {
      case FieldProperty(f)          => app >> ".[[" >> f >> "]]"
      case ComponentProperty(name)   => app >> "." >> name
      case IndexProperty(index)      => app >> "[" >> index >> "]"
      case IntrinsicProperty(intr)   => app >> ".[[" >> intr >> "]]"
      case NonterminalProperty(name) => app >> "the |" >> name >> "| of"
    }

  // intrinsics
  given intrRule: Rule[Intrinsic] = (app, intr) =>
    val Intrinsic(base, props) = intr
    app >> "%" >> base
    props.map(app >> "." >> _)
    app >> "%"

  // types
  given typeRule: Rule[Type] = (app, ty) => app >> ty.name

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // list with named separator
  private def listNamedSepRule[T](
    left: String = "",
    namedSep: String = "",
    right: String = "",
  )(using tRule: Rule[T]): Rule[List[T]] = (app, list) =>
    val len = list.length
    app >> left
    for ((x, k) <- list.zipWithIndex) app >> (k match {
      case 0                 => ""
      case _ if k == len - 1 => (if (len > 2) ", " else " ") + namedSep + " "
      case _                 => ", "
    }) >> x
    app >> right

  // append locations
  private def withLoc[T <: Locational](rule: Rule[T]): Rule[T] = (app, elem) =>
    given Rule[Option[Loc]] = (app, locOpt) =>
      locOpt.fold(app)(app >> " @ " >> _.toString)
    rule(app, elem)
    if (location) app >> elem.loc else app

  // verbs
  private def isStr(neg: Boolean, single: Boolean = true): String =
    val res = if (single) " is " else " are "
    if (neg) res + "not " else res
  private def hasStr(neg: Boolean): String =
    if (neg) " does not have " else " has "
}
