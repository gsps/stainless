/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

trait RustgenAnalysis extends AbstractAnalysis {
  import RustgenRun.Result
  import RustgenReport.Record

  val program: StainlessProgram
  val sources: Set[Identifier] // set of functions that were considered for the analysis
  val results: Seq[Result]

  private lazy val records = results map { case Result(fd, status, time) =>
    val textStatus = status match {
      case RustgenRun.UnsupportedFeature(error) => RustgenReport.UnsupportedFeature(error)
      case RustgenRun.Translated(rustUnit) => RustgenReport.Translated(rustUnit)
    }
    Record(fd.id, fd.getPos, textStatus, time)
  }

  override type Report = RustgenReport
  override val name = RustgenComponent.name
  override def toReport = new RustgenReport(records, sources)
}

