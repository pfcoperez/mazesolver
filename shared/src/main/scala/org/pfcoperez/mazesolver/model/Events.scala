package org.pfcoperez.mazesolver.model

object Events {

  sealed trait Event
  case class Claim(position: (Int, Int), territory: Int) extends Event
  case class Fusion(territoryA: Int, territoryB: Int) extends Event
  case object ExplorarionFinished extends Event

}
