package esmeta.util

import esmeta.util.BaseUtils.*
import esmeta.util.{Loc, Pos}
import java.io.*
import java.math.MathContext.UNLIMITED
import java.nio.charset.Charset
import scala.util.parsing.combinator.JavaTokenParsers

/** basic parsers */
trait BasicParsers extends JavaTokenParsers {
  // parse from file
  def fromFileWithParser[T](f: String, parser: Parser[T]): T = {
    var fileName = File(f).getCanonicalPath
    val fs = FileInputStream(File(f))
    val sr = InputStreamReader(fs, Charset.forName("UTF-8"))
    val in = BufferedReader(sr)
    val result = errHandle(parser, parseAll(parser, in))
    in.close; sr.close; fs.close
    result
  }

  /** white spaces with comments */
  lazy val whiteSpaceWithComment =
    """(\s|/\*+[^*]*\*+(?:[^/*][^*]*\*+)*/|//[^\u000A\u000D\u2028\u2029]*)+""".r

  // parse with error message
  def errHandle[T](parser: Parser[T], result: ParseResult[T]): T = result match
    case Success(result, _) => result
    case err                => error(s"[$parser] $err")

  // parse
  def parseBy[T](parser: Parser[T])(str: String): T =
    errHandle(parser, parseAll(parser, str))
  def parse[T](str: String)(using parser: Parser[T]): T =
    parseBy(parser)(str)

  // string literal
  lazy val string = ("\"[\u0000-\u000F]\"".r | stringLiteral) ^^ {
    case s =>
      StringContext processEscapes s.substring(1, s.length - 1)
  }

  // line terminator
  lazy val newline = "\r?\n|\r|\f".r

  // any word
  lazy val word = "\\w+".r

  // camel case
  lazy val camel = "[A-Z][a-zA-Z0-9]+".r

  // boolean
  lazy val bool = "true" ^^^ true | "false" ^^^ false

  // integers
  lazy val integer = "(0|-?[1-9]\\d*)".r
  lazy val int = integer ^^ { _.toInt }
  lazy val long = integer ^^ { _.toLong }

  // numbers
  lazy val number = "[+-]?(0|[1-9][0-9]*)(\\.[0-9]+)?".r
  lazy val double = number ^^ { _.toDouble }
  lazy val bigInt = integer ^^ { BigInt(_) }
  lazy val codeUnit = integer ^^ { _.toInt.toChar }
  lazy val decimal = number ^^ { BigDecimal(_, UNLIMITED) }

  trait From[T](parser: Parser[T]) {
    def fromFile(str: String): T =
      fromFileWithParser(str, parser)
    def from(str: String): T =
      parseBy[T](parser)(str)
  }

  // locations
  lazy val pos: Parser[Pos] = int ~ (":" ~> int) ~ ("(" ~> int <~ ")") ^^ {
    case l ~ c ~ o => Pos(l, c, o)
  }
  lazy val loc: Parser[Loc] = {

    lazy val alphaInt: Parser[Int] = {
      lazy val lower = "[a-z]".r
      lazy val upper = "[A-Z]".r

      lower ^^ {
        _.charAt(0).toInt - ('a'.toInt) + 1
      } | upper ^^ {
        _.charAt(0).toInt - ('A'.toInt) + 1
      }
    }

    lazy val romanInt: Parser[Int] = {
      lazy val chars = "[ivxlcdmIVXLCDM]+".r
      chars ^? RomanNumeralDecoder.apply.unlift
    }

    // step $stepString, rangeString
    // (step 3.e, 9:10-177)

    lazy val triple =
      lazy val fromRoman: Parser[List[Int]] =
        romanInt ~ opt("." ~> fromInt) ^^ {
          case i ~ is => i :: (is.getOrElse(Nil))
        }
      lazy val fromAlpha: Parser[List[Int]] =
        alphaInt ~ opt("." ~> fromRoman) ^^ {
          case i ~ is => i :: (is.getOrElse(Nil))
        }
      lazy val fromInt: Parser[List[Int]] =
        int ~ opt("." ~> fromAlpha) ^^ {
          case i ~ is => i :: (is.getOrElse(Nil))
        }
      fromInt

    lazy val stepTriple = ("step" ~> triple <~ ",")

    lazy val range: Parser[(Pos, Pos)] = pos ~ ("-" ~> pos) ^^ {
      case s ~ e => (s, e)
    }

    lazy val steps = "(" ~> opt(stepTriple) ~ range <~ ")" ^^ {
      case ss ~ (s -> e) => Loc(s, e, steps = ss.getOrElse(Nil))
    }

    steps
  }

  // ---------------------------------------------------------------------------
  // Source Location
  // ---------------------------------------------------------------------------
  abstract class LocationalParser[+T <: Locational] extends Parser[T]
  def locationed[T <: Locational](p: => Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(in: Input) =
        val trimmed = trimInput(in)
        p(trimmed) match
          case s @ Success(res, rest) =>
            Success(
              res.setLoc(trimmed, rest),
              rest,
            )
          case ns: NoSuccess => ns
    }

  /** trim unused whitespace for position */
  protected def trimInput[T](in: Input): Input =
    val offset = in.offset
    val start = handleWhiteSpace(in.source, offset)
    in.drop(start - offset)

  /** implicit conversion from scala parser's Input to util.Pos */
  protected given Conversion[Input, Pos] with
    def apply(in: Input): Pos = Pos(in.pos.line, in.pos.column, in.offset)

  /* extensions for Parser for replacing `^^!`
   *
   * NOTE: Unlike the ^^^ operator, it always computes the parsing result when
   * the parsing process succeeds. If the parsing result requires an automatic
   * insertion of location info in abstract algorithms (`_ <: Locational`),
   * please use the ^^! operator.
   *
   * $ var count = 0
   * $ lazy val p = "" ^^^ { count += 1 }
   * $ parse(p, ""); parse(p, ""); parse(p, ""); count
   * result: 1
   *
   * $ var count = 0
   * $ lazy val p = "" ^^! { count += 1 }
   * $ parse(p, ""); parse(p, ""); parse(p, ""); count
   * result: 3
   */
  extension [T](p: Parser[T]) {
    def ^^![S](v: => S): Parser[S] = Parser { in => p(in).map(x => v) }
  }
}
