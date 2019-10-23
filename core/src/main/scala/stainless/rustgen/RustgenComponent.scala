/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import ast.Trees.{Program => RustProgram}

import io.circe._

import java.io.File
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

import scala.language.existentials

object DebugSectionRustgen extends inox.DebugSection("rustgen")

object optRustgenOutPath
    extends inox.StringOptionDef("rustgen-out", "", "The file to output the rust program into.")

/**
  * Rustgen Component
  *
  * TODO(gsps): Explain
  */
object RustgenComponent extends Component { self =>
  override val name = "rustgen"
  override val description = "Generate rust code from stainless programs"

  override type Report = RustgenReport
  override type Analysis = RustgenAnalysis

  override val lowering = inox.transformers.SymbolTransformer(new transformers.TreeTransformer {
    val s: extraction.trees.type = extraction.trees
    val t: extraction.trees.type = extraction.trees
  })

  override def run(pipeline: extraction.StainlessPipeline)(implicit ctx: inox.Context) = {
    new RustgenRun(pipeline)
  }
}

object RustgenRun {
  import stainless.trees._

  sealed abstract class TranslationStatus
  case class Translated(rustProgram: RustProgram) extends TranslationStatus
  case class UnsupportedFeature(error: String) extends TranslationStatus

  case class Result(status: TranslationStatus, time: Long)
}

class RustgenRun(override val pipeline: extraction.StainlessPipeline)(
    override implicit val context: inox.Context
) extends {
  override val component = RustgenComponent
  override val trees: stainless.trees.type = stainless.trees
} with ComponentRun {

  import trees._
  import component.{Report, Analysis}
  import RustgenRun._

  override def parse(json: Json): Report = RustgenReport.parse(json)

  private implicit val debugSection = DebugSectionRustgen

  private[stainless] def execute(
      _functions: Seq[Identifier],
      symbols: Symbols
  ): Future[Analysis] = {
    import context._

    val p = inox.Program(trees)(symbols)
    import p.{symbols => _, _}

    val gen = new generator.Generator(context, symbols)

    def outputProgram(outputPath: String, program: RustProgram): Unit = {
      val outputFile = new File(outputPath).getAbsoluteFile
      reporter.info(s"Outputting program to $outputFile")
      assert(outputFile.getParentFile.exists)

      {
        val programContents = program.show()
        Files.write(outputFile.toPath, programContents.getBytes(StandardCharsets.UTF_8))
      }
    }

    def shouldIgnoreSort(sort: ADTSort): Boolean = {
      sort.flags contains Synthetic
    }
    def shouldIgnoreFunction(fd: FunDef): Boolean = {
      fd.flags contains Synthetic
    }

    val sorts = symbols.sorts.values.filterNot(shouldIgnoreSort).map(_.id).toSeq
    val functions = symbols.functions.values.filterNot(shouldIgnoreFunction).map(_.id).toSeq
    reporter.debug(s"Processing ${sorts.size} sorts: ${sorts mkString ", "}")
    reporter.debug(s"Processing ${functions.size} functions: ${functions mkString ", "}")

    def runGenerator(): TranslationStatus = {
      reporter.info(s"Running generator ...")

      val sortDefs = sorts.map(symbols.getSort)
      val funDefs = functions.map(symbols.getFunction)

      val status = Try(gen(sortDefs, funDefs)) match {
        case Failure(exc) =>
          // exc.printStackTrace()
          UnsupportedFeature(exc.getMessage())
        case Success(rustProgram) => Translated(rustProgram)
      }

      reporter.info(s"Result:")

      status match {
        case UnsupportedFeature(error) => reporter.warning(" => UNSUPPORTED FEATURE")
        case Translated(rustProgram)   => reporter.info(" => SUCCESSFUL")
      }

      val optError = status match {
        case UnsupportedFeature(error) => Some(error)
        case _                         => None
      }

      optError.foreach(error => reporter.warning(s"  $error"))

      val optBody = status match {
        case Translated(rustProgram) => Some(rustProgram)
        case _                       => None
      }

      optBody.foreach(
        rustProgram =>
          reporter.whenDebug(DebugSectionRustgen) { debug =>
            val symbols = rustProgram.symbols
            debug(s"Generated ${symbols.enums.size} enums and ${symbols.functions.size} functions")
          }
      )

      status
    }

    def tryRunGenerator(): Result = {
      val (time, tryStatus) = timers.rustgen.emit.runAndGetTime {
        runGenerator()
      }
      tryStatus match {
        case Failure(e)      => reporter.internalError(e)
        case Success(status) => Result(status, time)
      }
    }

    val genResult = tryRunGenerator()

    genResult.status match {
      case Translated(rustProgram) =>
        context.options.findOption(optRustgenOutPath) foreach {
          outputProgram(_, rustProgram)
        }
      case _ =>
    }

    Future.successful(new RustgenAnalysis {
      override val program = p
      override val sources = sorts.toSet ++ functions.toSet
      override val result = genResult
    })
  }
}
