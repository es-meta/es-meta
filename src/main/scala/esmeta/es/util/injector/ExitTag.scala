package esmeta.es.util.injector

import esmeta.error.*
import esmeta.state.*
import esmeta.util.BaseUtils.*

/** exit status tag */
trait ExitTag:
  override def toString: String = this match
    case NormalTag                   => s"normal"
    case TimeoutTag                  => s"timeout"
    case SpecErrorTag(error, cursor) => s"spec-error: $cursor"
    case TranspileFailTag            => s"transpile-failure"
    case ThrowValueTag(value: Value) => s"throw-value: $value"
    case ThrowErrorTag(errorName, msg) =>
      s"throw-error: ${errorName}${msg.map(msg => s"($msg)").getOrElse("")}"
  def equivalent(that: ExitTag): Boolean = (this, that) match
    case (_: ThrowValueTag, _: ThrowValueTag)               => true
    case (ThrowErrorTag(name1, _), ThrowErrorTag(name2, _)) => name1 == name2
    case _                                                  => this == that
object ExitTag:
  /** Get exit tag from exit status */
  def apply(st: => State): ExitTag = try {
    st(GLOBAL_RESULT) match
      case Undef => NormalTag
      case addr: Addr =>
        st(addr) match
          case ListObj(Vector(addr: DynamicAddr)) =>
            st(addr)(Str("Prototype")) match
              case NamedAddr(errorNameRegex(errorName)) =>
                ThrowErrorTag(errorName, None)
              case _ => ThrowValueTag(addr)
          case ListObj(Vector(v)) => ThrowValueTag(v)
          case obj                => error(s"unexpected exit status: $obj")
      case v => error(s"unexpected exit status: $v")
  } catch {
    case _: TimeoutException   => TimeoutTag
    case e: InterpreterErrorAt => SpecErrorTag(e.error, e.cursor)
  }

  /** Get exit tag by parsing */
  def apply(tag: => String): Option[ExitTag] = optional {
    val specErrorPattern = "spec-error: .*".r
    val throwValuePattern = "throw-value: .*".r
    val throwErrorPattern = "throw-error: (\\w+).*".r
    tag match {
      case "normal"                => NormalTag
      case "timeout"               => TimeoutTag
      case specErrorPattern()      => ???
      case "transpile-failure"     => TranspileFailTag
      case throwValuePattern()     => ThrowValueTag(Str(""))
      case throwErrorPattern(name) => ThrowErrorTag(name)
      case _                       => ???
    }
  }

  /** error name regex pattern */
  lazy val errorNameRegex = "INTRINSICS.([A-Z][a-z]+Error).prototype".r

/** normal exit */
case object NormalTag extends ExitTag

/** timeout */
case object TimeoutTag extends ExitTag

/** an error is thrown in specification */
case class SpecErrorTag(error: ESMetaError, cursor: Cursor) extends ExitTag

/** an error is thrown during transpilation */
case object TranspileFailTag extends ExitTag

/** an error is thrown with a ECMAScript value */
case class ThrowValueTag(value: Value) extends ExitTag

/** an error is thrown with an ECMAScript error */
case class ThrowErrorTag(
  errorName: String,
  msg: Option[String] = None,
) extends ExitTag
