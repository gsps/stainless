/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

// A generic way of deconstructing and reconstructing trees
object TreeDeconstructor {
  import Trees._

  type Builder[T <: Tree] =
    (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type]) => T
  type Deconstructed[T <: Tree] =
    (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type], Builder[T])

  protected final val NoIdentifiers: Seq[Identifier] = Seq()
  protected final val NoVariables: Seq[Variable] = Seq()
  protected final val NoExpressions: Seq[Expr] = Seq()
  protected final val NoTypes: Seq[Type] = Seq()

  // Deconstructs a rust tree into its components and a recontruction function
  def unapply(tree: Tree): Option[Deconstructed[Tree]] = {
    tree match {
      case expr: Expr => Some(deconstruct(expr))
      case tpe: Type => Some(deconstruct(tpe))
      case _ =>
        throw new NotImplementedError(s"Unsupported expression '$tree' (${tree.getClass})")
    }
  }

  /* Expressions */

  def deconstruct(expr: Expr): Deconstructed[Expr] = {
    expr match {
      case MissingExpr(tpe) =>
        (NoIdentifiers, NoVariables, NoExpressions, Seq(tpe),
          (_, _, _, tps) => MissingExpr(tps.head))
      case v @ Variable(_, _, _) =>
        (NoIdentifiers, Seq(v), NoExpressions, NoTypes,
          (_, vs, _, _) => vs.head)
      case lit: Literal[_] =>
        (NoIdentifiers, NoVariables, NoExpressions, NoTypes,
          (_, _, _, _) => lit)
      case Struct(id, tps, args) =>
        (Seq(id), NoVariables, args, tps,
          (ids, _, es, tps) => Struct(ids.head, tps, es))
      case Enum(id, tps, args) =>
        (Seq(id), NoVariables, args, tps,
          (ids, _, es, tps) => Enum(ids.head, tps, es))
      case Tuple(exprs) =>
        (NoIdentifiers, NoVariables, exprs, NoTypes,
          (_, _, es, _) => Tuple(es))
      case TupleSelect(expr, index, arity) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => TupleSelect(es.head, index, arity))
      case Let(vd, value, body) =>
        (NoIdentifiers, Seq(vd.toVariable), Seq(value, body), NoTypes,
          (_, vs, es, _) => Let(vs.head.toVal, es(0), es(1)))
      case expr: MatchExpr =>
        deconstructMatch(expr)
      case FunctionInvocation(fun, args) =>
        (Seq(fun), NoVariables, args, NoTypes,
          (ids, _, es, _) => FunctionInvocation(ids.head, es))
      case MethodInvocation(method, recv, args) =>
        (Seq(method), NoVariables, recv +: args, NoTypes,
          (ids, _, es, _) => MethodInvocation(ids.head, es.head, es.tail))
      case IfExpr(cond, thenn, elze) =>
        (NoIdentifiers, NoVariables, Seq(cond, thenn, elze), NoTypes,
          (_, _, es, _) => IfExpr(es(0), es(1), es(2)))
      case Error(tpe, description) =>
        (NoIdentifiers, NoVariables, NoExpressions, Seq(tpe),
          (_, _, _, tps) => Error(tps.head, description))

      case LabelledBlock(label, body) =>
        (Seq(label), NoVariables, Seq(body), NoTypes,
          (ids, _, es, _) => LabelledBlock(ids.head, es.head))
      case Break(label, arg) =>
        (Seq(label), NoVariables, Seq(arg), NoTypes,
          (ids, _, es, _) => Break(ids.head, es.head))
      case Sequence(expr1, expr2) =>
        (NoIdentifiers, NoVariables, Seq(expr1, expr2), NoTypes,
          (_, _, es, _) => Sequence(es(0), es(1)))

      case Reference(expr) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => Reference(es.head))
      case Dereference(expr) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => Dereference(es.head))

      case RcNew(expr) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => RcNew(es.head))
      case RcClone(expr) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => RcClone(es.head))
      case RcAsRef(expr) =>
        (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
          (_, _, es, _) => RcAsRef(es.head))
    }
  }

  /* Match expressions (taken almost verbatim from stainless Deconstructors) */

  def deconstructMatch(expr: MatchExpr): Deconstructed[MatchExpr] = {
    val MatchExpr(scrut, cases) = expr
    val (cids, cvs, ces, ctps, crecons) = deconstructCases(cases)
    (cids, cvs, scrut +: ces, ctps, (ids, vs, es, tps) => {
      val newScrut +: nes = es
      MatchExpr(newScrut, crecons(ids, vs, nes, tps))
    })
  }

  /** Rebuild a pattern from the given set of identifiers, variables, expressions, types and subpatterns */
  protected type PatternBuilder = (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type], Seq[Pattern]) => Pattern

  /** Extracted subtrees from a pattern as well as a "builder" */
  protected type DeconstructedPattern = (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type], Seq[Pattern], PatternBuilder)

  def deconstruct(pattern: Pattern): DeconstructedPattern = pattern match {
    case WildcardPattern(binder) =>
      (Seq(), binder.map(_.toVariable).toSeq, Seq(), Seq(), Seq(), (_, vs, _, _, _) => {
        WildcardPattern(vs.headOption.map(_.toVal))
      })
    case LiteralPattern(binder, lit) =>
      (Seq(), binder.map(_.toVariable).toSeq, Seq(lit), Seq(), Seq(), (_, vs, es, _, _) => {
        LiteralPattern(vs.headOption.map(_.toVal), es.head.asInstanceOf[Literal[_]])
      })
    case StructPattern(binder, id, subs) =>
      (Seq(id), binder.map(_.toVariable).toSeq, Seq(), Seq(), subs, (ids, vs, _, _, pats) => {
        StructPattern(vs.headOption.map(_.toVal), ids.head, pats)
      })
    case TuplePattern(binder, subs) =>
      (Seq(), binder.map(_.toVariable).toSeq, Seq(), Seq(), subs, (_, vs, _, _, pats) => {
        TuplePattern(vs.headOption.map(_.toVal), pats)
      })
    
  }

  /** Rebuild a match case from the given set of identifiers, variables, expressions and types */
  protected type CasesBuilder = (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type]) => Seq[MatchCase]

  /** Extracted subtrees from a match case as well as a "builder" */
  protected type DeconstructedCases = (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type], CasesBuilder)

  protected def deconstructCases(cases: Seq[MatchCase]): DeconstructedCases = {
    def rec(p: Pattern): (
      Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type],
      (Seq[Identifier], Seq[Variable], Seq[Expr], Seq[Type]) => Pattern
    ) = {
      val (ids, vs, es, tps, pats, reconsP) = deconstruct(p)
      val prec = pats.map(pat => (pat, rec(pat)))
      (
        ids ++ prec.flatMap(_._2._1),
        vs ++ prec.flatMap(_._2._2),
        es ++ prec.flatMap(_._2._3),
        tps ++ prec.flatMap(_._2._4),
        (nids, nvs, nes, ntps) => {
          val (outerIds, innerIds) = nids.splitAt(ids.size)
          val (outerVs, innerVs) = nvs.splitAt(vs.size)
          val (outerEs, innerEs) = nes.splitAt(es.size)
          val (outerTps, innerTps) = ntps.splitAt(tps.size)

          var rids = innerIds
          var rvs = innerVs
          var res = innerEs
          var rtps = innerTps
          val newPats = for ((pat, (ids, vs, es, tps, reconsPat)) <- prec) yield {
            val (currIds, nextIds) = rids.splitAt(ids.size)
            rids = nextIds

            val (currVs, nextVs) = rvs.splitAt(vs.size)
            rvs = nextVs

            val (currEs, nextEs) = res.splitAt(es.size)
            res = nextEs

            val (currTps, nextTps) = rtps.splitAt(tps.size)
            rtps = nextTps

            reconsPat(currIds, currVs, currEs, currTps).setPos(pat)
          }

          reconsP(outerIds, outerVs, outerEs, outerTps, newPats).setPos(p)
        }
      )
    }

    val recCases = for (caze <- cases) yield {
      val (pids, pvs, pes, ptps, precons) = rec(caze.pattern)
      (
        caze.getPos,
        caze.pattern.getPos,
        caze.optGuard.isDefined,
        pids,
        pvs,
        caze.optGuard.toSeq ++ (caze.rhs +: pes),
        ptps,
        precons
      )
    }

    (
      recCases.flatMap(_._4),
      recCases.flatMap(_._5),
      recCases.flatMap(_._6),
      recCases.flatMap(_._7),
      (ids, vs, es, tps) => {
        var rids = ids
        var rvs = vs
        var res = es
        var rtps = tps
        for ((cazePos, patternPos, hasGuard, ids, vs, es, tps, precons) <- recCases) yield {
          val (currIds, nextIds) = rids.splitAt(ids.size)
          rids = nextIds

          val (currVs, nextVs) = rvs.splitAt(vs.size)
          rvs = nextVs

          val (currEs, nextEs) = res.splitAt(es.size)
          res = nextEs

          val (currTps, nextTps) = rtps.splitAt(tps.size)
          rtps = nextTps

          if (hasGuard) {
            val guard +: rhs +: pes = currEs
            MatchCase(precons(currIds, currVs, pes, currTps).setPos(patternPos), Some(guard), rhs).setPos(cazePos)
          } else {
            val rhs +: pes = currEs
            MatchCase(precons(currIds, currVs, pes, currTps).setPos(patternPos), None, rhs).setPos(cazePos)
          }
        }
      }
    )
  }

  /* Types */

  def deconstruct(tpe: Type): Deconstructed[Type] = {
    tpe match {
      case NoType =>
        (NoIdentifiers, NoVariables, NoExpressions, NoTypes,
          (_, _, _, _) => NoType)
      case tpe: ErrorType =>
        (NoIdentifiers, NoVariables, NoExpressions, NoTypes,
          (_, _, _, _) => tpe)
      case HoleType(id) =>
        (Seq(id), NoVariables, NoExpressions, NoTypes,
          (ids, _, _, _) => HoleType(ids.head))
      case tpe: PrimitiveType =>
        (NoIdentifiers, NoVariables, NoExpressions, NoTypes,
          (_, _, _, _) => tpe)
      case StructType(id, tps) =>
        (Seq(id), NoVariables, NoExpressions, tps,
          (ids, _, _, tps) => StructType(ids.head, tps))
      case EnumType(id, tps) =>
        (Seq(id), NoVariables, NoExpressions, tps,
          (ids, _, _, tps) => EnumType(ids.head, tps))
      case TupleType(tps) =>
        (NoIdentifiers, NoVariables, NoExpressions, tps,
          (_, _, _, tps) => TupleType(tps))

      case RefType(tpe) =>
        (NoIdentifiers, NoVariables, NoExpressions, Seq(tpe),
          (_, _, _, tps) => RefType(tps.head))

      case RcType(tpe) =>
        (NoIdentifiers, NoVariables, NoExpressions, Seq(tpe),
          (_, _, _, tps) => RcType(tps.head))
    }
  }
}

object Deconstructors {
  import Trees._

  val deconstructor = TreeDeconstructor

  object Operator extends TreeExtractor[Expr] {
    def unapply(e: Expr): Option[(Seq[Expr], Seq[Expr] => Expr)] = {
      val (ids, vs, es, tps, /* flags,*/ builder) = deconstructor.deconstruct(e)
      Some(es, ess => builder(ids, vs, ess, tps /*, flags*/))
    }
  }

  object Invocation {
    def unapply(e: Expr): Option[(Identifier, Seq[Expr], Seq[Expr] => Expr)] = {
      e match {
        case FunctionInvocation(fun, args) =>
          Some(fun, args, es => FunctionInvocation(fun, es))
        case MethodInvocation(fun, recv, args) =>
          Some(fun, recv +: args, es => MethodInvocation(fun, es.head, es.tail))
        case _ =>
          None
      }
    }
  }

  object PatternBinder {
    def unapply(pat: Pattern): Some[(Option[ValDef], Option[ValDef] => Pattern)] = {
      val (ids, vs, es, tps, pats, recons) = deconstructor.deconstruct(pat)
      Some((vs.headOption.map(_.toVal),
        optBinder => recons(ids, optBinder.map(_.toVariable).toSeq, es, tps, pats)))
    }
  }
}
