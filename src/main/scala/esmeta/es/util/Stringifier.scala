package esmeta.es.util

import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.es.*

/** stringifier for ECMAScript */
class Stringifier(
  detail: Boolean,
  location: Boolean,
  grammar: Option[Grammar],
) {
  // elements
  given elemRule: Rule[ESElem] = (app, elem) =>
    elem match
      case elem: Script => scriptRule(app, elem)
      case elem: Ast    => astRule(app, elem)

  // ECMAScript script program
  given scriptRule: Rule[Script] = (app, script) => app >> script.code

  // abstract syntax tree (AST) values
  given astRule: Rule[Ast] = (app, ast) =>
    grammar match
      case Some(grammar) => grammarAstRule(app, (grammar, ast))
      case None          => basicAstRule(app, ast)

  // span information
  given locRule: Rule[Loc] = _ >> _.toString

  lazy val grammarAstRule: Rule[(Grammar, Ast)] = (app, pair) =>
    val (grammar, origAst) = pair
    val nameMap = grammar.nameMap
    def aux(ast: Ast): Unit = ast match
      case Lexical(name, str) => app >> str >> " "
      case Syntactic(name, args, rhsIdx, children) =>
        var cs = children
        for (symbol <- nameMap(name).rhsVec(rhsIdx).symbols) symbol match
          case Terminal(term)                          => app >> term >> " "
          case Empty | NoLineTerminator | _: Lookahead =>
          case _ =>
            cs.headOption match
              case Some(hd) => hd.map(aux); cs = cs.tail
              case _        => error(s"invalid AST: $origAst")
    aux(origAst)
    app

  lazy val basicAstRule: Rule[Ast] = (app, ast) =>
    ast match
      case Syntactic(name, args, rhsIdx, children) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "|" >> name >> "|"
        if (!args.isEmpty) app >> "[" >> args >> "]"
        app >> "<" >> rhsIdx >> ">"
        if (detail && ast.loc.isDefined) app >> ast.loc.get
        given Rule[Option[Ast]] = optionRule("<none>")
        if (detail) app.wrap("(", ")")(children.map(app :> _ >> ",")) else app
      case Lexical(name, str) =>
        app >> "|" >> name >> "|(" >> str >> ")"
        if (detail && ast.loc.isDefined) app >> ast.loc.get else app
}
