/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import generator.{rust => rt}

import io.circe._

import java.io.File
import java.nio.file.{ Files, Paths }
import java.nio.charset.StandardCharsets

import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }

import scala.language.existentials

object DebugSectionRustgen extends inox.DebugSection("rustgen")

object optRustgenOutDir extends inox.StringOptionDef("rustgen-out-dir",
  "", "A directory to output rust sources into.")

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
  case class Translated(rustTree: rt.Tree) extends TranslationStatus
  case class UnsupportedFeature(error: String) extends TranslationStatus

  case class Result(defn: Definition, status: TranslationStatus, time: Long)
}

class RustgenRun(override val pipeline: extraction.StainlessPipeline)
                (override implicit val context: inox.Context) extends {
  override val component = RustgenComponent
  override val trees: stainless.trees.type = stainless.trees
} with ComponentRun {

  import trees._
  import component.{Report, Analysis}
  import RustgenRun._

  override def parse(json: Json): Report = RustgenReport.parse(json)

  private implicit val debugSection = DebugSectionRustgen

  override def createFilter = RustgenCheckFilter(trees, context)

  private[stainless] def execute(functions: Seq[Identifier], symbols: Symbols): Future[Analysis] = {
    import context._

    val p = inox.Program(trees)(symbols)
    import p.{symbols => _, _}

    val gen = new generator.Generator()(context, symbols)

    // Translate a definition
    def translateDefinition(defn: Definition): TranslationStatus = {
      val fid = defn.id
      reporter.info(s"Translating ${fid}")

      val status = Try(gen.translate(defn)) match {
        case Failure(exc) => exc.printStackTrace; UnsupportedFeature(exc.getMessage())
        case Success(rustTree) => Translated(rustTree)
      }

      reporter.info(s"Result for ${fid.asString} @${defn.getPos}:")

      status match {
        case UnsupportedFeature(error) => reporter.warning(" => UNSUPPORTED FEATURE")
        case Translated(rustTree) => reporter.warning(" => SUCCESSFUL")
      }

      val optError = status match {
        case UnsupportedFeature(error) => Some(error)
        case _ => None
      }

      optError.foreach(error => reporter.warning(s"  $error"))

      val optBody = status match {
        case Translated(rustTree) => Some(rustTree)
        case _ => None
      }

      optBody.foreach(rustTree =>
        reporter.whenDebug(DebugSectionRustgen){ debug =>
          val output = rustTree.show(symbols).split("\n").mkString("\n  ")
          debug(s"Definition translates to:\n  $output")
        })

      status
    }

    def processDefinition(defn: Definition): Result = {
      val (time, tryStatus) = timers.rustgen.emit.runAndGetTime { translateDefinition(defn) }
      tryStatus match {
        case Failure(e) => reporter.internalError(e)
        case Success(status) => Result(defn, status, time)
      }
    }

    def outputProgram(outDirPath: String, program: rt.Program): Unit = {
      val outDir = new File(outDirPath).getAbsoluteFile
      reporter.info(s"Outputting program to $outDir")
      outDir.mkdirs()
      assert(outDir.isDirectory)
      program.modules.foreach { module =>
        val moduleFile = new File(outDir, s"${module.name}.rs")
        val moduleContents = module.show(symbols)
        Files.write(moduleFile.toPath, moduleContents.getBytes(StandardCharsets.UTF_8))
      }
    }

    def shouldIgnoreSort(sort: ADTSort): Boolean = {
      sort.flags contains Synthetic
    }

    val sorts = symbols.sorts.values.filterNot(shouldIgnoreSort).map(_.id).toSeq
    reporter.debug(s"Processing ${sorts.size} sorts: ${sorts mkString ", "}")
    reporter.debug(s"Processing ${functions.size} functions: ${functions mkString ", "}")

    val enumResults = sorts map (id => processDefinition(symbols.getSort(id)))
    val functionResults = functions map (id => processDefinition(symbols.getFunction(id)))

    context.options.findOption(optRustgenOutDir) foreach {
      outputProgram(_, gen.toProgram(symbols))
    }

    Future.successful(new RustgenAnalysis {
      override val program = p
      override val sources = sorts.toSet ++ functions.toSet
      override val results = enumResults ++ functionResults
    })
  }
}

