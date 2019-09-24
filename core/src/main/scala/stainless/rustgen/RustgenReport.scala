/* Copyright 2009-2019 EPFL, Lausanne */

package stainless
package rustgen

import stainless.utils.JsonConvertions._

import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

object RustgenReport {

  sealed abstract class Status
  case class UnsupportedFeature(error: String) extends Status
  case class Translated(rustTreeString: String) extends Status

  implicit val statusDecoder: Decoder[Status] = deriveDecoder
  implicit val statusEncoder: Encoder[Status] = deriveEncoder

  /**
   * Hold the information relative to the evaluation of a function.
   *
   * [[id]]: function's identifier
   * [[pos]]: function's position
   * [[status]]: result of the evaluation, see above
   * [[time]]: amount of time that the evaluation took
   */
  case class Record(id: Identifier, pos: inox.utils.Position, status: Status, time: Long)
    extends AbstractReportHelper.Record {
    override val derivedFrom = id
  }

  implicit val recordDecoder: Decoder[Record] = deriveDecoder
  implicit val recordEncoder: Encoder[Record] = deriveEncoder

  def parse(json: Json) = json.as[(Seq[Record], Set[Identifier])] match {
    case Right((records, sources)) => new RustgenReport(records, sources)
    case Left(error) => throw error
  }
}

class RustgenReport(val results: Seq[RustgenReport.Record], val sources: Set[Identifier])
  extends BuildableAbstractReport[RustgenReport.Record, RustgenReport] {
  import RustgenReport._

  override val encoder = recordEncoder

  override def build(results: Seq[Record], sources: Set[Identifier]) =
    new RustgenReport(results, sources)

  override val name = RustgenComponent.name

  override lazy val annotatedRows = results map {
    case Record(id, pos, status, time) =>
      RecordRow(id, pos, levelOf(status), Seq(descriptionOf(status)), time)
  }

  private lazy val totalTime = (results map { _.time }).sum
  private lazy val totalValid = results count { r => levelOf(r.status) == Level.Normal }
  private lazy val totalInvalid = results.size - totalValid

  override lazy val stats =
    ReportStats(results.size, totalTime, totalValid, validFromCache = 0, totalInvalid, unknown = 0)

  private def levelOf(status: Status) = status match {
    case Translated(_) => Level.Normal
    case _ => Level.Error
  }

  private def descriptionOf(status: Status): String = status match {
    case UnsupportedFeature(error) => "unsupported feature"
    case Translated(_) => "successful"
  }
}

