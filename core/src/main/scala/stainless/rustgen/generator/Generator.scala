/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")

// A stateful processor of Stainless ADTSorts and FunDefs that produces rust trees.
class Generator(ctx: inox.Context, symbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import ast.{Trees => rt}

  def apply(sorts: Seq[st.ADTSort], functions: Seq[st.FunDef]): rt.Program = {
    def checkWellTyped(program: rt.Program) = {
      implicit val symbols: rt.Symbols = program.symbols
      val illtyped = program.typer.checkWellTyped()
      if (illtyped.nonEmpty) {
        val lines = illtyped map { id =>
          val fd = program.symbols.getFunction(id)
          program.typer.getType(fd) match {
            case rt.ErrorType(reason) => s"  ${fd.id.fullName} @${fd.getPos}: ${reason.show}"
            case _                    => assert(false)
          }
        }
        ctx.reporter.internalError(
          s"Extraction phase produced ill-typed functions:\n  ${lines.mkString("\n  ")}"
        )
      }
    }

    def trim(program: rt.Program): rt.Program = {
      val symbols = program.symbols
      val newSymbols = rt.Symbols(
        symbols.structs,
        symbols.enums,
        symbols.functions.filterNot(_._2.flags.contains(rt.Library))
      )
      rt.Program(newSymbols)
    }

    val extraction = new ExtractionPhase(symbols)
    val extractionProgram = extraction(sorts, functions)
    checkWellTyped(extractionProgram)
    trim(extractionProgram)
  }
}
