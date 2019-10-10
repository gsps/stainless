/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import org.scalatest._
import scala.concurrent.duration._
import stainless.rustgen.RustgenRun.Translated
import stainless.rustgen.RustgenRun.UnsupportedFeature

class RustgenSuite extends ComponentTestSuite {

  val component = RustgenComponent

  protected lazy val tmpOutputPath = {
    import java.nio.file.Files
    val tmpPath = Files.createTempFile("rustgen-test-out", ".rs")
    tmpPath.toFile.deleteOnExit()
    tmpPath.toAbsolutePath
  }

  override def configurations =
    // = super.configurations.map {
    //   seq => optRustgenOutPath(tmpOutputPath.toString) +: seq
    // }
    Seq(Seq(optRustgenOutPath(tmpOutputPath.toString), inox.optSelectedSolvers(Set()), inox.optTimeout(300.seconds)))

  override protected def optionsString(options: inox.Options): String = "solvr=dummy"

  override def filter(ctx: inox.Context, name: String): FilterStatus = name match {
    case _ => super.filter(ctx, name)
  }

  testAll("rustgen/valid") { (analysis, reporter) =>
    assert(analysis.toReport.stats.validFromCache == 0, "no cache should be used for these tests")
    analysis.result.status match {
      case Translated(rustProgram) => assert(true)
      case UnsupportedFeature(error) => fail(s"The translation failed due to an unsupported feature:\n$error")
    }
    reporter.terminateIfError()
  }
}
