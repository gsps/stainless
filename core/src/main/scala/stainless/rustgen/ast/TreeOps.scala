/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import Trees._

import inox.utils.fixpoint

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

trait TreeExtractor[T <: Tree] {
  def unapply(t: T): Option[(Seq[T], Seq[T] => T)]
}

// Generic operations on rust trees (partially replicates inox.ast.GenTreeOps)
trait GenTreeOps {
  type T <: Tree

  val Deconstructor: TreeExtractor[T]

  /* Basic operations */

  def preTraversal(f: T => Unit)(t: T): Unit = {
    val rec = preTraversal(f) _
    val Deconstructor(ts, _) = t
    f(t)
    ts.foreach(rec)
  }

  def postTraversal(f: T => Unit)(t: T): Unit = {
    val rec = preTraversal(f) _
    val Deconstructor(ts, _) = t
    ts.foreach(rec)
    f(t)
  }

  def fold[S](f: (T, Seq[S]) => S)(t: T): S = {
    val rec = fold(f) _
    val Deconstructor(ts, _) = t
    f(t, ts.view.map(rec))
  }

  def postMap(f: T => Option[T], applyRec: Boolean = false)(e: T): T = {
    val rec = postMap(f, applyRec) _

    val Deconstructor(es, builder) = e
    val newEs = es.map(rec)
    val newV: T = {
      if ((newEs zip es).exists { case (bef, aft) => aft ne bef }) {
        builder(newEs).copiedFrom(e)
      } else {
        e.asInstanceOf[T]
      }
    }

    if (applyRec) {
      // Apply f as long as it returns Some()
      fixpoint { e: T =>
        f(e) getOrElse e
      }(newV)
    } else {
      f(newV) getOrElse newV
    }
  }

  /* Auxiliary operations */

  /** Collects a set of objects from all sub-expressions */
  def collect[S](matcher: T => Set[S])(e: T): Set[S] = {
    fold[Set[S]]({ (e, subs) =>
      matcher(e) ++ subs.flatten
    })(e)
  }
}

/* Concrete implementations */

object ExprOps extends GenTreeOps {
  type T = Expr
  val Deconstructor = Deconstructors.Operator

  /** Replaces bottom-up variables by looking up for them in a map */
  def replaceFromValDefs(substs: Map[ValDef, Expr], expr: Expr): Expr = {
    new TreeTransformer {
      override def transform(expr: Expr): Expr = expr match {
        case v: Variable => substs.getOrElse(v.toVal, super.transform(v))
        case _           => super.transform(expr)
      }
    }.transform(expr)
  }

  /** Counts for each ValDef how often it is referred to. **/
  def countValDefs(expr: Expr): Map[Identifier, Int] = {
    val counts: MutableMap[Identifier, Int] = MutableMap.empty.withDefaultValue(0)
    val valDefs: MutableSet[ValDef] = MutableSet.empty
    object transformer extends TreeTransformer {
      override def transform(vd: ValDef): ValDef = {
        valDefs.add(vd)
        vd
      }
      override def transform(expr: Expr): Expr = expr match {
        case v: Variable => counts(v.id) += 1; expr
        case _           => super.transform(expr)
      }
    }
    transformer.transform(expr)
    valDefs.map(vd => vd.id -> counts(vd.id)).toMap
  }
}
