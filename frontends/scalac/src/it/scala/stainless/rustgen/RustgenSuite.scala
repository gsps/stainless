/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import org.scalatest._
import scala.concurrent.duration._
import stainless.rustgen.RustgenRun.Translated
import stainless.rustgen.RustgenRun.UnsupportedFeature

import java.nio.file.{Files, Path}

class RustgenSuite extends ComponentTestSuite {

  val component = RustgenComponent

  protected lazy val tmpOutputPath: Path = {
    val tmpPath = Files.createTempFile("rustgen-test-out", ".rs")
    tmpPath.toFile.deleteOnExit()
    tmpPath.toAbsolutePath
  }

  override def configurations =
    Seq(
      Seq(
        optRustgenOutPath(tmpOutputPath.toString),
        inox.optSelectedSolvers(Set()),
        inox.optTimeout(300.seconds)
      )
    )

  override protected def optionsString(options: inox.Options): String = "solvr=dummy"

  override def filter(ctx: inox.Context, name: String): FilterStatus = name match {
    case _ => super.filter(ctx, name)
  }

  def testAndCompileAll(dir: String): Unit = {
    def checkRustgenOutput(analysis: component.Analysis, reporter: inox.Reporter): Unit = {
      assert(analysis.toReport.stats.validFromCache == 0, "no cache should be used for these tests")
      analysis.result.status match {
        case Translated(rustProgram) => assert(true)
        case UnsupportedFeature(error) =>
          fail(s"The translation failed due to an unsupported feature:\n$error")
      }
      reporter.terminateIfError()
    }

    testAll(dir) { (analysis, reporter) =>
      checkRustgenOutput(analysis, reporter)
      val compilation = RustcCompilation(tmpOutputPath)
      if (!compilation.isSuccess) {
        fail(s"Rustc reported errors when compiling rustgen output:\n${compilation.error.get}")
      }
    }
  }

  testAndCompileAll("rustgen/valid")
}

case class RustcCompilation(inputPath: Path, error: Option[String]) {
  def isSuccess: Boolean = error.isEmpty
}

object RustcCompilation {
  import scala.sys.process._
  val RustcBin = "rustc"

  def apply(inputPath: Path): RustcCompilation = {
    val (tmpDir, tmpFile) = (inputPath.getParent, inputPath.getFileName)
    val outputSb = new StringBuffer
    val outputLogger = ProcessLogger(outputSb.append(_))
    val exitCode = Process(RustcBin :: tmpFile.toString :: Nil, tmpDir.toFile) ! outputLogger
    val error = if (exitCode == 0) None else Some(outputSb.toString)
    RustcCompilation(inputPath, error)
  }
}
