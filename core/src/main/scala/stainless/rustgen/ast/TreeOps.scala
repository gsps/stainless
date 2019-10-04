/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package ast

// Generic operations on rust trees (partially replicates inox.ast.GenTreeOps)
object TreeOps {
  import Trees._

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