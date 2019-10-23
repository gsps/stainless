/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{IdentityProgramTransformer, TreeTransformer}
import ast.Trees._
import ast.ExprOps

class Idiomatization extends IdentityProgramTransformer {
  class Idiomatizer(implicit symbols: Symbols) extends TreeTransformer {
    var valDefUsage: Map[Identifier, Int] = Map.empty

    override def transform(fd: FunDef): FunDef = {
      valDefUsage = ExprOps.countValDefs(fd.body)
      super.transform(fd)
    }

    def isUnused(vd: ValDef): Boolean = valDefUsage(vd.id) == 0
    def isSubstitutable(vd: ValDef, value: Expr): Boolean = {
      // TODO: Only substitute when `vd` arose from inlining and is side-effect-free
      // valDefUsage(vd.id) == 1 && vd.flags.contains(InlineParam) && ExprOps.isPure(value)
      false
    }

    override def transform(e: Expr): Expr = {
      e match {
        case Let(vd, value, body) if isUnused(vd) =>
          Sequence(transform(value), transform(body)).copiedFrom(e)
        case Let(vd, value, body) if isSubstitutable(vd, value) =>
          val substs = Map(vd -> value)
          val ee = ExprOps.replaceFromValDefs(substs, body)
          ee
        case _ =>
          super.transform(e)
      }
    }
  }

  override def transformFunctions(
      functions: Seq[FunDef]
  )(implicit symbols: Symbols): Seq[FunDef] = {
    val idiomatizer = new Idiomatizer()
    functions.map(idiomatizer.transform)
  }
}
