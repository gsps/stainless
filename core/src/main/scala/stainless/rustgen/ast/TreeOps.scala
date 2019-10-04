/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

import Trees._

import inox.utils.fixpoint

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

  def postMap(f: T => Option[T], applyRec : Boolean = false)(e: T): T = {
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
      fixpoint { e : T => f(e) getOrElse e } (newV)
    } else {
      f(newV) getOrElse newV
    }
  }
}

/* Concrete implementations */

object ExprOps extends GenTreeOps {
  type T = Expr
  val Deconstructor = Deconstructors.Operator
}
