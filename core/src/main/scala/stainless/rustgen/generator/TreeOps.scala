/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

// Generic operations on rust trees (partially replicates inox.ast.GenTreeOps)
trait TreeOps {
  import rust._

  /* A generic way of deconstructing and reconstructing trees */
  
  object Deconstructor {
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
        case expr: Expr => deconstruct(expr)
        case tpe: Type => deconstruct(tpe)
        case _ =>
          throw new NotImplementedError(s"Unsupported expression '$tree' (${tree.getClass})")
      }
    }

    def deconstruct(expr: Expr): Option[Deconstructed[Expr]] = {
      Some(expr match {
        case MissingExpr(tpe) =>
          (NoIdentifiers, NoVariables, NoExpressions, Seq(tpe),
            (_, _, _, tps) => MissingExpr(tps.head))
        case v @ Variable(id, tpe) =>
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

        case Reference(expr) =>
          (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
            (_, _, es, _) => Reference(es.head))

        case RcNew(expr) =>
          (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
            (_, _, es, _) => RcNew(es.head))
        case RcClone(expr) =>
          (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
            (_, _, es, _) => RcClone(es.head))
        case RcDeref(expr) =>
          (NoIdentifiers, NoVariables, Seq(expr), NoTypes,
            (_, _, es, _) => RcDeref(es.head))
      })
    }

    def deconstructMatch(expr: MatchExpr): Deconstructed[MatchExpr] = {
      ???
    }

    def deconstruct(tpe: Type): Option[Deconstructed[Type]] = {
      Some(tpe match {
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
      })
    }
  }


  /* Basic operations */

  def preTraversal(f: Tree => Unit)(t: Tree): Unit = {
    val rec = preTraversal(f) _
    val Deconstructor(_, _, ts, _, _) = t
    f(t)
    ts.foreach(rec)
  }

  def postTraversal(f: Tree => Unit)(t: Tree): Unit = {
    val rec = preTraversal(f) _
    val Deconstructor(_, _, ts, _, _) = t
    ts.foreach(rec)
    f(t)
  }

  def fold[T](f: (Tree, Seq[T]) => T)(t: Tree): T = {
    val rec = fold(f) _
    val Deconstructor(_, _, ts, _, _) = t
    f(t, ts.view.map(rec))
  }
}