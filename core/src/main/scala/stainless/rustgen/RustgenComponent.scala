/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import io.circe._

import scala.concurrent.Future
import scala.util.{ Success, Failure }

import scala.language.existentials

object DebugSectionRustgen extends inox.DebugSection("rustgen")

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

  sealed abstract class FunctionStatus
  case class Translated(rustUnit: String) extends FunctionStatus
  case class UnsupportedFeature(error: String) extends FunctionStatus

  case class Result(fd: FunDef, status: FunctionStatus, time: Long)
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

    // Build an evaluator once and only if there is something to evaluate
    lazy val translator: FunDef => Either[String, String] = ???

    // Translate a function
    def translateFunction(fd: FunDef): FunctionStatus = {
      val fid = fd.id
      reporter.info(s"Translating ${fid}")

      val status = translator(fd) match {
        case Left(error) => UnsupportedFeature(error)
        case Right(rustUnit) => Translated(rustUnit)
      }

      reporter.info(s"Result for ${fid.asString} @${fd.getPos}:")

      status match {
        case UnsupportedFeature(error) => reporter.warning(" => UNSUPPORTED FEATURE")
        case Translated(rustUnit) => reporter.warning(" => SUCCESSFUL")
      }

      val optError = status match {
        case UnsupportedFeature(error) => Some(error)
        case _ => None
      }

      optError.foreach(error => reporter.warning(s"  $error"))

      val optBody = status match {
        case Translated(rustUnit) => Some(rustUnit)
        case _ => None
      }

      optBody.foreach(rustUnit =>
        reporter.info(s"Function translates to:\n  ${rustUnit.split("\n").mkString("\n  ")}"))

      status
    }

    // Measure how long it takes to determine the function' status
    def processFunction(fd: FunDef): Result = {
      val (time, tryStatus) = timers.rustgen.emit.runAndGetTime { translateFunction(fd) }
      tryStatus match {
        case Failure(e) => reporter.internalError(e)
        case Success(status) => Result(fd, status, time)
      }
    }

    reporter.debug(s"Processing ${functions.size} parameterless functions: ${functions mkString ", "}")

    Future.successful(new RustgenAnalysis {
      override val program = p
      override val sources = functions.toSet
      override val results = functions map (id => processFunction(symbols.getFunction(id)))
    })
  }
}

