package org.pfcoperez.mazesolver.datastructures

import BitVector._

import scala.collection.immutable.Vector

class BitVector private (
    val size: Int,
    emptyValue: Boolean = false
)(
    private val data: Vector[Byte] =
      Vector.fill(ceilDiv(size, 8))(emptyCell(emptyValue))
) {
  def set(bitPosition: Int): BitVector = {
    val (cell, cellPosition, positionInBit) = cellAndShift(bitPosition)
    new BitVector(size)(
      data.updated(cellPosition, (cell | 1 << positionInBit).toByte)
    )
  }

  def isSet(bitPosition: Int): Boolean = {
    val (cell, _, positionInBit) = cellAndShift(bitPosition)
    (cell & (1 << positionInBit)) > 0
  }

  private def cellAndShift(position: Int): (Byte, Int, Int) = {
    val cellPosition = position / 8
    (data(cellPosition), cellPosition, position % 8)
  }
}

object BitVector {
  def fill(n: Int)(v: Boolean): BitVector = {
    new BitVector(n, v)()
  }

  private def ceilDiv(x: Int, y: Int): Int = x / y + (if (x % y > 0) 1 else 0)
  private def emptyCell(emptyValue: Boolean): Byte = if (emptyValue) 0 else 1
}
