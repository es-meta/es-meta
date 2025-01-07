package esmeta.parser

import esmeta.util.BaseUtils.*
import esmeta.util.Locational
import scala.util.parsing.input.*

trait LAParsers extends Lexer {
  // debugging mode
  val debug: Boolean

  // failed parser
  val failed: Parser[Nothing] = failure("")

  // first terms
  case class FirstTerms(
    possibleEmpty: Boolean = false,
    ts: Set[String] = Set(),
    nts: Map[String, Lexer] = Map(),
  ) {
    def makeEmptyPossible: FirstTerms = copy(possibleEmpty = true)
    def +(that: FirstTerms): FirstTerms = FirstTerms(
      this.possibleEmpty || that.possibleEmpty,
      this.ts ++ that.ts,
      this.nts ++ that.nts,
    )
    def +(t: String): FirstTerms = copy(ts = ts + t)
    def +(nt: (String, Lexer)): FirstTerms = copy(nts = nts + nt)
    def ~(that: => FirstTerms): FirstTerms = if (!possibleEmpty) this
    else
      FirstTerms(
        that.possibleEmpty,
        this.ts ++ that.ts,
        this.nts ++ that.nts,
      )
    def parser: Lexer = Skip ~> Parser { rawIn =>
      val base =
        if (possibleEmpty) phrase(empty)
        else failed
      val t = TERMINAL.filter(ts contains _)
      record(
        nts.foldLeft(base | t)(_ | _._2),
        rawIn.asInstanceOf[EPackratReader[Char]],
      )
    }
    override def toString: String = {
      ts.map("\"" + _ + "\"") ++
      nts.map(_._1) ++
      (if (possibleEmpty) List("ε") else Nil)
    }.mkString("[", ", ", "]")
  }

  // empty first terms
  val emptyFirst: FirstTerms = FirstTerms(possibleEmpty = true)

  //  no first terms
  val noFirst: FirstTerms = FirstTerms()

  // lookahead parsers
  class LAParser[+T](
    val parser: FirstTerms => Parser[T],
    val first: FirstTerms,
  ) {
    def ~[U](that: => LAParser[U]): LAParser[~[T, U]] = LAParser(
      follow => this.parser(that.first ~ follow) ~ that.parser(follow),
      this.first ~ that.first,
    )

    def ~>[U](that: => LAParser[U]): LAParser[U] = LAParser(
      follow => this.parser(that.first ~ follow) ~> that.parser(follow),
      this.first ~ that.first,
    )

    def <~[U](that: => LAParser[U]): LAParser[T] = LAParser(
      follow => this.parser(that.first ~ follow) <~ that.parser(follow),
      this.first ~ that.first,
    )

    def |[U >: T](that: LAParser[U]): LAParser[U] =
      if (that eq MISMATCH) this
      else
        LAParser(
          follow => this.parser(follow) | that.parser(follow),
          this.first + that.first,
        )

    def ^^[U](f: T => U): LAParser[U] = LAParser(
      follow => this.parser(follow) ^^ f,
      this.first,
    )

    def ^^^[U](v: => U): LAParser[U] = LAParser(
      follow => this.parser(follow) ^^^ v,
      this.first,
    )

    def apply(follow: FirstTerms, in: EPackratReader[Char]): ParseResult[T] =
      parser(follow)(in)

    def unary_- : LAParser[Unit] = LAParser(
      follow => not(parser(follow)),
      emptyFirst,
    )

    def unary_+ : LAParser[T] = LAParser(
      follow => guard(parser(follow)),
      emptyFirst,
    )
  }

  // always match
  lazy val MATCH: LAParser[String] =
    log(LAParser(follow => "" <~ guard(follow.parser), emptyFirst))("MATCH")

  // always mismatch
  lazy val MISMATCH: LAParser[Nothing] =
    log(LAParser(follow => failed, noFirst))("MISMATCH")

  // optional parsers
  def opt[T](p: => LAParser[T]): LAParser[Option[T]] = p ^^ {
    Some(_)
  } | MATCH ^^^ None

  // memoization of lookahead parsers
  case class ParseCase[+T](
    parser: LAParser[T],
    follow: FirstTerms,
    pos: Position,
  )
  protected def memo[T](p: LAParser[T]): LAParser[T] = new LAParser(
    follow =>
      Parser { rawIn =>
        val in = rawIn.asInstanceOf[EPackratReader[Char]]
        val c = ParseCase(p, follow, in.pos)
        val data = in.data
        data.cache.get(c) match {
          case Some(res) => res.asInstanceOf[ParseResult[T]]
          case None =>
            val res = record(p.parser(follow), in)
            data.cache += c -> res
            res
        }
      },
    p.first,
  )

  // location
  // handle whitespace in span info
  override def locationed[T <: Locational](p: => Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(in: Input) =
        // TODO handle white space
        val trimmed = trimInput(in)
        p(in) match
          case s @ Success(res, rest) =>
            new Success(
              res.setLoc(trimmed, rest),
              rest,
            )
          case ns: NoSuccess => ns
    }
  def locationed[T <: Locational](p: LAParser[T]): LAParser[T] = new LAParser(
    follow => locationed(p.parser(follow)),
    p.first,
  )

  // logging
  var keepLog: Boolean = true
  def log[T](p: LAParser[T])(name: String): LAParser[T] = if (debug) {
    new LAParser(
      follow =>
        Parser { rawIn =>
          val in = rawIn.asInstanceOf[EPackratReader[Char]]
          val stopMsg =
            s"""----------------------------------------
               |trying $name
               |with $follow at [${in.pos}]
               |
               |${in.pos.longString}
               | 
               |parser> """.stripMargin
          if (keepLog) stop(stopMsg) match
            case "j" | "jump" =>
              keepLog = false
              val r = p(follow, in)
              println(name + " --> " + r)
              keepLog = true
              r
            case "q" | "quit" =>
              keepLog = false
              p(follow, in)
            case _ =>
              val r = p(follow, in)
              println(name + " --> " + r)
              r
          else p(follow, in)
        },
      p.first,
    )
  } else p

  // stop message
  protected def stop(msg: String): String = {
    print(msg)
    val res = scala.io.StdIn.readLine
    res
  }

  // Parse character reader `in` with parser `p`
  def parse[T](p: LAParser[T], in: Reader[Char]): ParseResult[T] = {
    p(emptyFirst, new EPackratReader(in))
  }

  // Parse character sequence `in` with parser `p`
  def parse[T](p: LAParser[T], in: java.lang.CharSequence): ParseResult[T] =
    parse(p, new CharSequenceReader(in))

  // Parse reader `in` with parser `p`
  def parse[T](p: LAParser[T], in: java.io.Reader): ParseResult[T] =
    parse(p, new PagedSeqReader(PagedSeq.fromReader(in)))

  // record parsing process
  protected def record[T](
    parser: Parser[T],
    in: EPackratReader[Char],
  ): ParseResult[T]

  // terminal lexer
  protected val TERMINAL: Parser[String]
}
