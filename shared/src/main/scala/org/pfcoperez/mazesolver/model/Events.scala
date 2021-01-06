package org.pfcoperez.mazesolver.model

object Events {

  sealed trait Event
  object Event {
    private val ClaimRegex =
      """Claim\(\(([0-9]+)\s*,([0-9]+)\s*\),([0-9]+)\)""".r
    private val FusionRegex =
      """Fusion\(([0-9]+),([0-9]+)\)""".r

    def unapply(eventStr: String): Option[Event] = eventStr match {
      case ClaimRegex(iStr, jStr, territoryStr) =>
        Some(Claim((iStr.toInt, jStr.toInt), territoryStr.toInt))
      case FusionRegex(aStr, bStr) =>
        Some(Fusion(aStr.toInt, bStr.toInt))
      case _ => None
    }
  }
  case class Claim(position: (Int, Int), territory: Int) extends Event
  case class Fusion(territoryA: Int, territoryB: Int) extends Event
  case object ExplorarionFinished extends Event

}
