/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")

object optRustgenPrintTypes
    extends inox.FlagOptionDef("rustgen-print-types", false)

// A stateful processor of Stainless ADTSorts and FunDefs that produces rust trees.
class Generator(ctx: inox.Context, symbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import ast.{Trees => rt}

  val debugPrinterOpts: ast.PrinterOptions = {
    val printTypes = ctx.options.findOptionOrDefault(optRustgenPrintTypes)
    ast.PrinterOptions(printTypes)
  }

  def apply(sorts: Seq[st.ADTSort], functions: Seq[st.FunDef]): rt.Program = {
    def checkWellTyped(program: rt.Program) = {
      implicit val symbols: rt.Symbols = program.symbols
      val illtyped = symbols.typer.checkWellTyped()
      if (illtyped.nonEmpty) {
        val lines = illtyped map { id =>
          val fd = program.symbols.getFunction(id)
          val rt.ErrorType(reason) = symbols.typer.getType(fd)
          s"    ${fd.id.fullName} @${reason.blamed.getPos}: ${reason.show()}"
        }
        val programStr = program.show(debugPrinterOpts)
        ctx.reporter.internalError(
          s"Extraction phase produced ill-typed functions:\n${lines.mkString("\n")}\n\n$programStr"
        )
      }
    }

    def trim(program: rt.Program): rt.Program = {
      val symbols = program.symbols
      val newSymbols = rt.Symbols(
        symbols.structs,
        symbols.enums,
        symbols.functions.filterNot(_._2.flags.contains(rt.Library)),
        symbols.strictTyping
      )
      rt.Program(newSymbols)
    }

    val extraction = new ExtractionPhase(symbols)
    val extractedProgram = extraction(sorts, functions)
    checkWellTyped(extractedProgram)

    val matchFlattenedProgram = new MatchFlattening().transform(extractedProgram)
    checkWellTyped(matchFlattenedProgram)

    val typeLoweredProgram = new TypeLowering().transform(matchFlattenedProgram)
    checkWellTyped(typeLoweredProgram)

    trim(typeLoweredProgram)
  }
}
