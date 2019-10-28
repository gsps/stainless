/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen
package generator

object DebugSectionRustgenGenerator extends inox.DebugSection("rustgen-generator")

object optRustgenPrint extends inox.OptionDef[Set[String]] {
  import inox.OptionParsers._

  val name = "rustgen-print"
  val default = Set[String]()
  val parser: OptionParser[Set[String]] = setParser(stringParser)

  val usageRhs = "p1,p2,..."
}

object optRustgenPrintTypes extends inox.FlagOptionDef("rustgen-print-types", false)
object optRustgenPrintImplicit extends inox.FlagOptionDef("rustgen-print-implicit", false)
object optRustgenPrintLibrary extends inox.FlagOptionDef("rustgen-print-library", false)

// A stateful processor of Stainless ADTSorts and FunDefs that produces rust trees.
class Generator(ctx: inox.Context, symbols: stainless.trees.Symbols) {
  import stainless.{trees => st}
  import ast.{Trees => rt}

  val debugPrinterOpts: ast.PrinterOptions = {
    val printTypes = ctx.options.findOptionOrDefault(optRustgenPrintTypes)
    val printImplicit = ctx.options.findOptionOrDefault(optRustgenPrintImplicit)
    val printLibrary = ctx.options.findOptionOrDefault(optRustgenPrintLibrary)
    ast.PrinterOptions(printTypes, printImplicit, printLibrary)
  }
  val printPhases: Set[String] = ctx.options.findOptionOrDefault(optRustgenPrint)

  def apply(sorts: Seq[st.ADTSort], functions: Seq[st.FunDef]): rt.Program = {
    def ensureWellFormed(phaseName: String, program: rt.Program) = {
      implicit val symbols: rt.Symbols = program.symbols
      lazy val programStr = program.show(debugPrinterOpts)
      val illtyped = symbols.typer.checkWellTyped()
      if (illtyped.nonEmpty) {
        val lines = illtyped map { id =>
          val fd = program.symbols.getFunction(id)
          val rt.ErrorType(reason) = symbols.typer.getType(fd)
          s"    ${fd.id.fullName} @${reason.blamed.getPos}: ${reason.show()}"
        }
        ctx.reporter.internalError(
          s"Phase '$phaseName' produced ill-typed functions:\n${lines.mkString("\n")}\n\n$programStr"
        )
      }
      if (printPhases.contains(phaseName)) {
        ctx.reporter.info(s"--- Program after Rustgen phase $phaseName: ---\n\n$programStr")
      }
    }

    val extraction = new ExtractionPhase(symbols)
    val extractedProgram = extraction(sorts, functions)
    ensureWellFormed("extraction", extractedProgram)

    val matchFlattenedProgram = new MatchFlattening().transform(extractedProgram)
    ensureWellFormed("matchFlattening", matchFlattenedProgram)

    val typeLoweredProgram = new TypeLowering().transform(matchFlattenedProgram)
    ensureWellFormed("typeLowering", typeLoweredProgram)

    val idiomatizedProgram = new Idiomatization().transform(typeLoweredProgram)
    ensureWellFormed("idiomatization", idiomatizedProgram)

    idiomatizedProgram
  }
}
