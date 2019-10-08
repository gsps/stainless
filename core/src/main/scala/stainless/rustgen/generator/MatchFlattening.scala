/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

import ast.{IdentityProgramTransformer, TreeTransformer}
import ast.Trees._
import ast.Deconstructors.Invocation

class MatchFlattening extends IdentityProgramTransformer {
  class MatchFlattener(implicit symbols: Symbols) extends TreeTransformer {
    override def transform(e: Expr): Expr = {
      e match {
        case mtch: MatchExpr =>
          // TODO
          super.transform(mtch)

        case _ =>
          super.transform(e)
      }
    }
  }

  override def transformFunctions(
      functions: Seq[FunDef]
  )(implicit symbols: Symbols): Seq[FunDef] = {
    val matchFlattener = new MatchFlattener()
    functions.map(matchFlattener.transform)
  }
}
