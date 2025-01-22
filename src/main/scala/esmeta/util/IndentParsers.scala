package esmeta.util

import scala.util.parsing.combinator.*
import scala.util.parsing.input.{Reader, Position}
import scala.collection.mutable

/** parsers for indentations defined with packrat parsing */
trait IndentParsers extends BasicParsers with EPackratParsers {
  // extended data for indentation
  case class Data(
    indents: List[Int] = Nil,
    steps: List[Int] = Nil,
    needUppercase: Boolean = false,
  ) extends DataType { def next = Data(indents, steps, false) }
  val defaultData = Data()

  /** end of line */
  val EOL = "\n|$".r

  /** treat only spaces as white spaces */
  override val whiteSpace = "( |\n *$)+".r

  /** a next parser */
  val next: Parser[Unit] = new Parser[Unit] {
    val name = "next"
    def apply(in: Input) = in match {
      case in: In =>
        newlineWithSpaces(in) match {
          case Success(expected, newIn: In) =>
            val data @ Data(indents, steps, needUppercase) = in.data
            indents match {
              case indent :: _ if expected != indent =>
                Failure(s"expected $expected but $indent indentations", in)
              case _ =>
                val newSteps = steps match
                  case Nil       => Nil
                  case s :: rest => (s + 1) :: rest
                Success(
                  (),
                  newIn.copy(newData = data.copy(indents, newSteps, false)),
                )
            }
          case Success(_, newIn) => Failure("not an EPackratReader", newIn)
          case fail: NoSuccess   => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a indent parser */
  val indent: Parser[Unit] = new Parser[Unit] {
    val name = "indent"
    def apply(in: Input) = in match {
      case in: In =>
        val data @ Data(indents, steps, needUppercase) = in.data
        (indents, newlineWithSpaces(in)) match {
          case (Nil, Success(indent, _)) => Success((), push(in, indent))
          case (prevIndent :: _, Success(indent, _)) =>
            if (prevIndent < indent) Success((), push(in, indent))
            else Failure(s"> $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a dedent parser */
  val dedent: Parser[Unit] = new Parser[Unit] {
    val name = "dedent"
    def apply(in: Input) = in match {
      case in: In =>
        val data @ Data(indents, steps, needUppercase) = in.data
        (pop(in), newlineWithSpaces(in)) match {
          case (None, _) => Failure("no indentation for dedent", in)
          case (Some((prevIndent, newIn)), Success(indent, _)) =>
            if (indent < prevIndent) Success((), newIn)
            else Failure(s"< $prevIndent expected but $indent indentations", in)
          case (_, fail: NoSuccess) => fail
        }
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** turn on needUppercase symbols */
  val upper: Parser[Unit] = new Parser[Unit] {
    def apply(in: Input) = in match {
      case in: In =>
        Success((), in.copy(newData = in.data.copy(needUppercase = true)))
      case in => Failure("not an EPackratReader", in)
    }
  }

  /** a parser that matches a literal string */
  override implicit def literal(s: String): Parser[String] =
    val superLiteral = super.literal
    new Parser[String] {
      def apply(in: Input) = superLiteral(in match {
        case in: In if in.data.needUppercase && s.length > 0 =>
          s"${s.head.toUpper}${s.substring(1)}"
        case _ => s
      })(in)
    }

  // ---------------------------------------------------------------------------
  // locational parser
  // ---------------------------------------------------------------------------
  override def locationed[T <: Locational](
    p: => Parser[T],
  ): LocationalParser[T] =
    new LocationalParser {
      def apply(in: Input) = in match
        case in: In =>
          val trimmed = trimInput(in).asInstanceOf[In]
          p(trimmed) match
            case s @ Success(res, rest) =>
              Success(
                res.setLoc(trimmed, rest, trimmed.data.steps.reverse),
                rest,
              )
            case ns: NoSuccess => ns
        case in => Failure("not an IndentReader", in)
    }

  // trim unused whitespace for position
  override protected def trimInput[T](input: Input): Input =
    val in = input.asInstanceOf[In]
    val trimmed = super.trimInput(in).asInstanceOf[In]
    trimmed.copy(newData =
      trimmed.data.copy(needUppercase = in.data.needUppercase),
    )

  /** implicit conversion from parsers to locational parsers */
  implicit def parser2loc[T <: Locational](
    p: => Parser[T],
  ): LocationalParser[T] = {
    val packrat = parser2packrat(p)
    locationed(packrat)
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // a parser for newline with spaces
  private val newlineWithSpaces: EPackratParser[Int] =
    "\n *".r ^^ { _.length - 1 } | "$".r ^^^ 0

  // push a new indentation
  private def push(in: In, indent: Int): In =
    val data @ Data(indents, steps, _) = in.data
    in.copy(newData = data.copy(indent :: indents, 0 :: steps, false))

  // pop an indentation
  private def pop(in: In): Option[(Int, In)] =
    val data @ Data(indents, steps, needUppercase) = in.data
    indents match
      case Nil => None
      case indent :: remain =>
        Some(indent, in.copy(newData = data.copy(remain, steps.tail, false)))

  // packrat reader for characters
  protected type In = EPackratReader[Char]
  protected def handleReader[T](
    in: Input,
  )(f: In => ParseResult[T]): ParseResult[T] = in match
    case in: In => f(in)
    case in     => Failure("not an EPackratReader", in)
}
