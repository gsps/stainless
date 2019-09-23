/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import utils.CheckFilter

trait RustgenCheckFilter extends CheckFilter {
  import trees._

  override def shouldBeChecked(fd: FunDef): Boolean =
    super.shouldBeChecked(fd)
}

object RustgenCheckFilter {
  def apply(t: ast.Trees, ctx: inox.Context): RustgenCheckFilter {
    val trees: t.type
  } = new RustgenCheckFilter {
    override val context = ctx
    override val trees: t.type = t
  }
}

