/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

trait RustgenAnalysis extends AbstractAnalysis {
  import RustgenRun.Result
  import RustgenReport.Record

  val program: StainlessProgram
  val sources: Set[Identifier]
  val result: Result

  private lazy val records = Seq(result) map { case Result(status, time) =>
    val textStatus = status match {
      case RustgenRun.UnsupportedFeature(error) => RustgenReport.UnsupportedFeature(error)
      case RustgenRun.Translated(rustProgram) => RustgenReport.Translated()
    }
    Record(inox.FreshIdentifier("<noname>"), inox.utils.NoPosition, textStatus, time)
  }
  override type Report = RustgenReport
  override val name = RustgenComponent.name
  override def toReport = new RustgenReport(records, sources)
}

