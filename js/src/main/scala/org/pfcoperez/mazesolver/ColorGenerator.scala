package org.pfcoperez.mazesolver

object ColorGenerator {

  def generateHsl(n: Int, maxInRange: Int): String = {
    assert(n <= maxInRange)
    assert(n >= 0)
    val h = (360.0 * n.toFloat / maxInRange.toFloat).toInt
    s"hsl($h, 100%, 72%)"
  }

}
