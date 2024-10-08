package esmeta.spec

import esmeta.ir
import esmeta.lang.Type
import esmeta.spec.util.Parser
import esmeta.ty.*

/** algorithm heads */
sealed trait Head extends SpecElem {
  val retTy: Type

  /** get original parameters */
  def originalParams: List[Param] = this match
    case abs: AbstractOperationHead       => abs.params
    case numeric: NumericMethodHead       => numeric.params
    case sdo: SyntaxDirectedOperationHead => sdo.withParams
    case con: ConcreteMethodHead          => con.params
    case int: InternalMethodHead          => int.params
    case builtin: BuiltinHead             => builtin.params

  /** get function parameters */
  def funcParams: List[Param] = this match
    case head: AbstractOperationHead => head.params
    case head: NumericMethodHead     => head.params
    case head: ConcreteMethodHead    => head.receiverParam :: head.params
    case head: InternalMethodHead    => head.receiverParam :: head.params
    case head: SyntaxDirectedOperationHead =>
      val thisTy = head.target match
        case Some(target) =>
          AstSingleT(target.lhsName, target.idx, target.subIdx)
        case None => AstT
      Param("this", Type(thisTy)) :: head.withParams
    case head: BuiltinHead =>
      List(
        Param(ir.THIS_STR, Type(ESValueT)),
        Param(ir.ARGS_LIST_STR, Type(ListT(ESValueT))),
        Param(ir.NEW_TARGET_STR, Type(ConstructorT || UndefT)),
      )

  /** get function name */
  def fname: String = this match
    case head: AbstractOperationHead =>
      head.name
    case head: NumericMethodHead =>
      s"${head.baseTy.normalizedName}::${head.name}"
    case head: SyntaxDirectedOperationHead =>
      val Target = SyntaxDirectedOperationHead.Target
      val pre = head.target.fold("<DEFAULT>") {
        case Target(lhsName, idx, subIdx, _) => s"$lhsName[$idx,$subIdx]"
      }
      s"$pre.${head.methodName}"
    case head: ConcreteMethodHead =>
      s"${head.receiverParam.ty.normalizedName}.${head.concMethodName}"
    case head: InternalMethodHead =>
      s"${head.receiverParam.ty.normalizedName}.${head.methodName}"
    case head: BuiltinHead =>
      val str = head.path.toString
      val patched =
        if (str startsWith "Generator.prototype")
          str.replace(
            "Generator.prototype",
            "GeneratorFunction.prototype.prototype",
          )
        else if (str startsWith "AsyncGenerator.prototype")
          str.replace(
            "AsyncGenerator.prototype",
            "AsyncGeneratorFunction.prototype.prototype",
          )
        else str
      s"INTRINSICS.$patched"
}

/** abstract operation (AO) heads */
case class AbstractOperationHead(
  isHostDefined: Boolean,
  name: String,
  params: List[Param],
  retTy: Type,
) extends Head

/** numeric method heads */
case class NumericMethodHead(
  baseTy: Type,
  name: String,
  params: List[Param],
  retTy: Type,
) extends Head

/** syntax-directed operation (SDO) heads */
case class SyntaxDirectedOperationHead(
  target: Option[SdoHeadTarget],
  methodName: String,
  isStatic: Boolean,
  withParams: List[Param],
  retTy: Type,
) extends Head
object SyntaxDirectedOperationHead:
  case class Target(
    lhsName: String,
    idx: Int,
    subIdx: Int,
    rhsParams: List[Param],
  ) extends SpecElem
type SdoHeadTarget = SyntaxDirectedOperationHead.Target

/** concrete method heads */
case class ConcreteMethodHead(
  concMethodName: String,
  receiverParam: Param,
  params: List[Param],
  retTy: Type,
) extends Head

/** internal method heads */
case class InternalMethodHead(
  methodName: String,
  receiverParam: Param,
  params: List[Param],
  retTy: Type,
) extends Head

/** built-in heads */
case class BuiltinHead(
  path: BuiltinPath,
  params: List[Param],
  retTy: Type,
) extends Head
enum BuiltinPath extends SpecElem:
  case Base(name: String)
  case NormalAccess(base: BuiltinPath, name: String)
  case Getter(base: BuiltinPath)
  case Setter(base: BuiltinPath)
  case SymbolAccess(base: BuiltinPath, symbol: String)
  case YetPath(name: String)
object BuiltinPath extends Parser.From(Parser.builtinPath)
