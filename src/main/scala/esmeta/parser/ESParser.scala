package esmeta.parser

import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.util.parsing.combinator.*
import scala.util.parsing.input.*
import scala.util.matching.Regex

/** ECMAScript parser */
case class ESParser(
  val grammar: Grammar,
  val debug: Boolean = false,
) extends LAParsers {

  /** get a parser using its name with boolean arguments
    *
    * @param name
    *   a parser name
    * @param args
    *   boolean arguments
    */
  def apply(name: String, args: List[Boolean] = Nil): AstFrom =
    val parser = parsers(name)(args)
    new AstFrom {
      def fromFile(filename: String): Ast =
        if (debug) println(debugWelcome)
        val ast = parse(parser, fileReader(filename)).get
        updateFilename(ast, filename)
        ast
      def from(str: String): Ast =
        if (debug) println(debugWelcome)
        parse(parser, str).get
      def fromFileWithCode(filename: String): (Ast, String) =
        if (debug) println(debugWelcome)
        val res = parse(parser, fileReader(filename))
        val ast = res.get
        updateFilename(ast, filename)
        (ast, res.next.source.toString)
      def fromWithCode(str: String): (Ast, String) =
        if (debug) println(debugWelcome)
        val res = parse(parser, str)
        (res.get, res.next.source.toString)
    }

  // parsers
  lazy val parsers: Map[String, ESParser[Ast]] = (for {
    prod <- grammar.prods
  } yield {
    val name = prod.lhs.name
    val parser =
      if (prod.kind == ProductionKind.Syntactic)
        // TODO handle in a more general way for indirect left-recursion
        if (name == "CoalesceExpressionHead") handleLR
        else getParser(prod)
      else (args: List[Boolean]) => nt(name, lexers(name, 0 /* TODO args */ ))
    name -> parser
  }).toMap

  /** recursively update the filename in the location information of the AST */
  def updateFilename(ast: Ast, name: String): Unit =
    for (loc <- ast.loc) loc.filename = Some(name)
    ast match
      case ast: Syntactic => ast.children.map(_.map(updateFilename(_, name)))
      case _              =>

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // debugger welcome message
  val debugWelcome: String =
    """Please type command:
      |  - "s" or "skip" to the skip the current target
      |  - "q" or "quit" to quit the debugging mode
      |  - other commands to perform one step""".stripMargin
  //
  // create syntactic object
  private def syntactic(
    name: String,
    args: List[Boolean],
    idx: Int,
    children: Vector[Option[Ast]],
  ): Syntactic =
    val syn = Syntactic(name, args, idx, children)
    // set parent edge
    for { childOpt <- children }
      childOpt.foreach(_.parent = Some(syn))
    syn
  def withLoc(
    syn: Syntactic,
    astStart: Ast,
    astEnd: Ast,
  ): Syntactic = syn.setLoc(astStart mergeLoc astEnd)

  // get a parser
  private def getParser(prod: Production): ESParser[Ast] = memo(args =>
    locationed {
      val Production(lhs, _, _, rhsVec) = prod
      val Lhs(name, params) = lhs
      val argsSet = getArgs(params, args)

      val lrs = rhsVec.zipWithIndex
        .filter { case (r, _) => isLR(name, r) }
        .map { case (r, i) => getSubParsers(name, args, argsSet, i, r) }

      val nlrs = rhsVec.zipWithIndex
        .filter { case (r, _) => !isLR(name, r) }
        .map { case (r, i) => getParsers(name, args, argsSet, i, r) }

      if (lrs.isEmpty) log(nlrs.reduce(_ | _))(name)
      else
        log(resolveLR(locationed(nlrs.reduce(_ | _)), lrs.reduce(_ | _)))(name)
    },
  )

  // get a sub parser for direct left-recursive cases
  private def getSubParsers(
    name: String,
    args: List[Boolean],
    argsSet: Set[String],
    idx: Int,
    rhs: Rhs,
  ): FLAParser[Ast] =
    val pre: String = s"$name[$idx]: $name "
    val cursor = Range(0, pre.length + 7).map(_ => " ").reduce(_ + _) + "^"
    log(if (rhs.available(argsSet)) {
      val base: LAParser[List[Option[Ast]]] = MATCH ^^^ Nil
      rhs.symbols
        .drop(1)
        .foldLeft(base)(appendParser(name, _, _, argsSet)) ^^ {
        case cs =>
          (base: Ast) =>
            val children = Some(base) :: cs.reverse
            withLoc(
              syntactic(name, args, idx, children.toVector),
              base,
              cs.flatten.headOption.getOrElse(base),
            )
      }
    } else MISMATCH)(s"$pre${rhs.symbols.drop(1)}$LINE_SEP$cursor")

  // get parsers
  private def getParsers(
    name: String,
    args: List[Boolean],
    argsSet: Set[String],
    idx: Int,
    rhs: Rhs,
  ): LAParser[Ast] =
    val pre: String = s"$name[$idx]: "
    val cursor = Range(0, pre.length + 7).map(_ => " ").reduce(_ + _) + "^"
    log(if (rhs.available(argsSet)) {
      val base: LAParser[List[Option[Ast]]] = MATCH ^^^ Nil
      rhs.symbols.foldLeft(base)(appendParser(name, _, _, argsSet)) ^^ {
        case cs => syntactic(name, args, idx, cs.toVector.reverse)
      }
    } else MISMATCH)(s"$pre$rhs$LINE_SEP$cursor")

  // append a parser
  private def appendParser(
    name: String,
    prev: LAParser[List[Option[Ast]]],
    symbol: Symbol,
    argsSet: Set[String],
    optional: Boolean = false,
  ): LAParser[List[Option[Ast]]] = symbol match
    case Terminal(")") if name == "DoWhileStatement" => prev <~ doWhileCloseT
    case Terminal(term)                              => prev <~ t(term)
    case symbol: NtBase =>
      lazy val parser = symbol match
        case Nonterminal(name, args) =>
          if (lexNames contains name) nt(name, lexers(name, 0 /* TODO args */ ))
          else parsers(name)(toBools(argsSet, args))
        case ButNot(base, cases) =>
          val name = s"$base \\ ${cases.mkString("(", ", ", ")")}"
          nt(name, getSymbolParser(symbol, argsSet))
        case ButOnlyIf(base, methodName, cond) => ???
      if (optional) prev ~ opt(parser) ^^ { case l ~ s => s :: l }
      else prev ~ parser ^^ { case l ~ s => Some(s) :: l }
    case Optional(symbol) => appendParser(name, prev, symbol, argsSet, true)
    case Lookahead(contains, cases) =>
      lazy val parser = cases
        .map(
          _.map(_ match {
            case Terminal(t) if t.matches("[a-z]+") => t <~ not(IDContinue)
            case symbol => getSymbolParser(symbol, argsSet)
          }).reduce(_ %% _),
        )
        .reduce(_ | _)
      lazy val lookahead = ntl(symbol.toString, parser)
      prev <~ (if (contains) +lookahead else -lookahead)
    case Empty            => prev
    case NoLineTerminator => prev <~ noLineTerminator
    case _                => throw UnexpectedSymbol(symbol)

  // a terminal lexer
  protected val TERMINAL: EPackratParser[String] =
    val ts = (for {
      prod <- grammar.prods
      if prod.kind == ProductionKind.Syntactic
      rhs <- prod.rhsVec
      t <- rhs.symbols.collect { case Terminal(t) => t }
    } yield t).toSet
    // XXX `x ?.1 : y` is `x ? .1 : y` but not `x ?. 1 : y`
    val trie = Trie(ts - "?.")
    def aux(
      trie: Trie,
      in: Input,
      prevRes: ParseResult[String],
      cur: String = "",
    ): ParseResult[String] =
      val res = if (trie.exist) Success(cur, in) else prevRes
      if (!in.atEnd && trie.children.contains(in.first))
        aux(
          trie.children(in.first),
          in.rest,
          res,
          cur + in.first,
        )
      else res
    val parser = Parser { in => aux(trie, in, Failure("", in)) }
    // XXX `x ?.1 : y` is `x ? .1 : y` but not `x ?. 1 : y`
    parser ||| ("?." <~ not("\\d".r))

  // automatic semicolon insertion
  private def insertSemicolon(reader: EPackratReader[Char]): Option[String] = {
    reader.data.rightmostFailedPos match {
      case Some((pos, rev)) =>
        val source = reader.source.toString
        val line = pos.line - 1
        val column = pos.column - 1
        val lines = source.replace("\r\n", "\n").split(Array('\n', '\r'))
        val revStr = rev.mkString

        // Interactive debugging for semicolon insertion
        if (debug && keepLog) {
          lines.zipWithIndex.foreach {
            case (x, i) => println(f"$i%4d: $x")
          }
          stop(s"line: $line, column: $column") match {
            case "q" => keepLog = false
            case _   =>
          }
        }

        lazy val curLine = lines(line)
        lazy val curChar = curLine.charAt(column)

        // insert semicolon right before the offending token
        lazy val insert = Some({
          if (line < lines.length && column < curLine.length) {
            val (pre, post) = curLine.splitAt(column)
            lines(line) = pre + ';' + post
            lines.mkString("\n")
          } else lines.mkString("\n") + "\n;"
        })

        // 2. The end of the input stream of tokens is encountered
        if (
          line >= lines.length ||
          (line == lines.length - 1 && column == curLine.length)
        ) return insert

        // A. Additional Rules
        // A semicolon is never inserted automatically if the semicolon
        // would then be parsed as an empty statement or if that semicolon
        // would become one of the two semicolons in the header of a for statement
        // TODO
        if (curChar == ';') return None

        // 1-1. The offending token is separated from the previous token
        //      by at least one LineTerminator
        if (!parse(strNoLineTerminator, revStr).successful) return insert

        // 1-2. The offending token is '}'
        if (curChar == '}') return insert

        // 1-3. The previous token is ')' and the inserted semicolon would then be
        //      parsed as the terminating semicolon of a do-while statement (13.7.2).
        reader.data.rightmostDoWhileClose match {
          case Some(doWhilePos) if doWhilePos == pos => return insert
          case _                                     =>
        }

        // 3. the restricted token is separated from the previous token
        //    by at least one LineTerminator, then a semicolon is automatically
        //    inserted before the restricted token.
        // TODO

        None
      case None => None
    }
  }

  // terminal parsers
  private val t = cached[String, LAParser[String]] {
    case t =>
      log(
        LAParser(
          follow => {
            Skip ~> {
              if (t.matches("[a-z]+")) t <~ not(IDContinue)
              else t
            } <~ +follow.parser
          },
          FirstTerms() + t,
        ),
      )(t)
  }
  private val doWhileCloseT: LAParser[String] = {
    val p = t(")")
    LAParser(
      follow =>
        Parser { rawIn =>
          val in = rawIn.asInstanceOf[EPackratReader[Char]]
          val c = ParseCase(p, follow, in.pos)
          val data = in.data
          data.cache.get(c) match {
            case Some(res) => res.asInstanceOf[ParseResult[String]]
            case None =>
              val res = recordDoWhileClose(p.parser(follow), in)
              data.cache += c -> res
              res
          }
        },
      p.first,
    )
  }
  private val nt = cached[(String, Lexer), LAParser[Lexical]] {
    case (name, nt) =>
      log(
        locationed(
          LAParser(
            follow =>
              (Skip ~> nt <~ +follow.parser) ^^ { case s => Lexical(name, s) },
            FirstTerms() + (name -> nt),
          ),
        ),
      )(name)
  }
  private val ntl = cached[(String, Lexer), LAParser[Lexical]] {
    case (name, nt) =>
      log(
        locationed(
          LAParser(
            follow => (Skip ~> nt) ^^ { case s => Lexical("", s) },
            FirstTerms(),
          ),
        ),
      )(name)
  }

  // parser that supports automatic semicolon insertions
  override def parse[T](p: LAParser[T], in: Reader[Char]): ParseResult[T] = {
    val MAX_ADDITION = 100
    val init: Either[ParseResult[T], Reader[Char]] = Right(in)
    (0 until MAX_ADDITION).foldLeft(init) {
      case (Right(in), _) =>
        val reader = EPackratReader(in)
        p(emptyFirst, reader) match {
          case (f: Failure) =>
            insertSemicolon(reader) match {
              case Some(str) =>
                if (debug && keepLog) {
                  println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
                  str
                    .replace("\r\n", "\n")
                    .split(Array('\n', '\r'))
                    .zipWithIndex
                    .foreach {
                      case (x, i) => println(f"$i%4d: $x")
                    }
                }
                Right(CharSequenceReader(str))
              case None => Left(f)
            }
          case r => Left(r)
        }
      case (res, _) => res
    } match {
      case Left(res) => res
      case _         => throw TooManySemicolonInsertion(MAX_ADDITION)
    }
  }

  // ECMAScript parsers
  type ESParser[+T] = List[Boolean] => LAParser[T]

  // memoization of parametric rules
  private def memo[T](f: ESParser[T]): ESParser[T] =
    cached(args => memo(f(args)))

  // resolve left recursions
  private type FLAParser[T] = LAParser[T => T]
  private def resolveLR[T <: Ast](
    f: LAParser[T],
    s: FLAParser[T],
  ): LAParser[T] = {
    lazy val p: FLAParser[T] = s ~ p ^^ {
      case b ~ f => (x: T) => f(b(x))
    } | MATCH ^^^ { (x: T) => x }
    f ~ p ^^ { case a ~ f => f(a) }
  }

  // record right-most field positions
  protected def record[T](
    parser: Parser[T],
    in: EPackratReader[Char],
  ): ParseResult[T] = {
    val data = in.data
    val res = parser(in)
    (res, data.rightmostFailedPos) match {
      case (f @ Failure(_, cur: EPackratReader[_]), Some((origPos, _)))
          if origPos < cur.pos =>
        data.rightmostFailedPos = Some((cur.pos, cur.rev))
      case (f @ Failure(_, cur: EPackratReader[_]), None) =>
        data.rightmostFailedPos = Some((cur.pos, cur.rev))
      case _ =>
    }
    res
  }

  // record right-most do-while close token positions
  protected def recordDoWhileClose[T](
    parser: Parser[T],
    in: EPackratReader[Char],
  ): ParseResult[T] = {
    val data = in.data
    val res = parser(in)
    (res, data.rightmostDoWhileClose) match {
      case (f @ Failure(_, cur: EPackratReader[_]), Some(origPos))
          if origPos < cur.pos =>
        data.rightmostDoWhileClose = Some(cur.pos)
      case (f @ Failure(_, cur: EPackratReader[_]), None) =>
        data.rightmostDoWhileClose = Some(cur.pos)
      case _ =>
    }
    res
  }

  // no LineTerminator parser
  lazy val noLineTerminator: LAParser[String] = log(
    LAParser(
      follow => strNoLineTerminator,
      emptyFirst,
    ),
  )("noLineTerminator")

  // get fixed length arguments
  private def getArgsN(
    name: String,
    args: List[Boolean],
    n: Int,
  ): List[Boolean] = {
    if (args.length == n) args
    else throw WrongNumberOfParserParams(name, args)
  }

  // check whether it is a left-recursive production
  private def isLR(name: String, rhs: Rhs): Boolean = (for {
    symbol <- rhs.symbols.headOption
    nt <- symbol.getNt
  } yield nt.name == name).getOrElse(false)

  // TODO more general rule
  // handle indirect left-recursive case
  private def handleLR: ESParser[Ast] = memo(args => {
    log(
      locationed(
        resolveLR(
          log(locationed(MATCH ~ parsers("BitwiseORExpression")(args) ^^ {
            case _ ~ x0 =>
              syntactic("CoalesceExpressionHead", args, 1, Vector(Some(x0)))
          }))("CoalesceExpressionHead1"),
          log(
            (MATCH <~ t("??")) ~ parsers("BitwiseORExpression")(args) ^^ {
              case _ ~ x0 => (
                (x: Ast) =>
                  val expr = withLoc(
                    syntactic(
                      "CoalesceExpression",
                      args,
                      0,
                      Vector(Some(x), Some(x0)),
                    ),
                    x,
                    x0,
                  )
                  withLoc(
                    syntactic(
                      "CoalesceExpressionHead",
                      args,
                      0,
                      Vector(Some(expr)),
                    ),
                    expr,
                    expr,
                  ),
              )
            },
          )("CoalesceExpressionHead0"),
        ),
      ),
    )("CoalesceExpressionHead")
  })
}
