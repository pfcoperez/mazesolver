package org.pfcoperez.mazesolver

object ColorGenerator {

  def generateHsl(n: Int, maxInRange: Int): String = {
    assert(n <= maxInRange)
    assert(n >= 0)
    val h = (360.0 * n.toFloat / maxInRange.toFloat).toInt
    s"hsl($h, 100%, 72%)"
  }

  def generateToneFrequency(n: Int, maxInRange: Int): Int = {
    val minHz = 200
    val maxHz = 1000
    val offset = ((maxHz - minHz) * (n.toFloat / maxInRange.toFloat)).toInt
    minHz + offset
  }

}
