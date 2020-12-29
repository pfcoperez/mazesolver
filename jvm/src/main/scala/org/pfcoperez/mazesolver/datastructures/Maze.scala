package org.pfcoperez.mazesolver.datastructures

import Maze._

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.Try

case class Maze(
    cells: Vector[Vector[Cell]]
) {
  assert(cells.size > 0)
  assert(cells.map(_.size).toSet.size == 1)

  def n: Int = cells.size
  def m: Int = cells.head.size

  def withinMaze(i: Int, j: Int): Boolean = {
    Seq(i -> n, j -> m).forall { case (x, limit) => x >= 0 && x < limit }
  }

  def update(i: Int, j: Int)(value: Cell): Option[Maze] = for {
    _ <- Some(())
    if withinMaze(i, j)
    row <- cells.lift(i)
    updatedRow = row.updated(j, value)
  } yield Maze(cells.updated(i, updatedRow))

  def get(i: Int, j: Int): Option[Cell] = for {
    row <- cells.lift(i)
    value <- row.lift(j)
  } yield value

  override def toString: String = cells
    .map { row =>
      row.mkString(" ")
    }
    .mkString("\n")
}

object Maze {
  sealed trait Cell {
    def binaryValue: Int
    override def toString: String = binaryValue.toString
  }
  object Cell {
    def apply(cellStr: String): Cell = {
      if (cellStr == Wall.toString) Wall
      else if (cellStr == Empty.toString) Empty
      else Claimed(cellStr.toInt)
    }
  }
  case object Empty extends Cell {
    def binaryValue: Int = -1
    override def toString: String = "."
  }
  case class Claimed(label: Int) extends Cell {
    assert(label >= 0)
    def binaryValue: Int = label
  }
  case object Wall extends Cell {
    def binaryValue: Int = -2
    override def toString: String = "#"
  }

  def fill(n: Int, m: Int)(element: Cell = Empty): Maze = {
    Maze(Vector.fill(n)(Vector.fill(m)(element)))
  }

  // Persistence interface

  def apply(file: File): Try[Maze] = load(file)

  def save(file: File)(maze: Maze): Try[Unit] = Try {
    val writer = new PrintWriter(file)
    writer.write(maze.toString)
    writer.close()
  }

  def fromLines(lines: Seq[String]): Try[Maze] = {
    val cells = lines.toVector.map { rawRow =>
      rawRow.split(" ").toVector.map(Cell.apply)
    }
    val loadedMaze = Maze(cells)
    val wrongRows = loadedMaze.cells.zipWithIndex
      .collect {
        case (row, line) if row.size != loadedMaze.m =>
          s"Line [$line] has [${row.size}] when first line determined m=[${loadedMaze.m}]"
      }

    Try {
      assert(
        wrongRows.isEmpty,
        s"All rows must have the same size:\n${wrongRows.mkString("\n")}"
      )

      loadedMaze
    }
  }

  def load(file: File): Try[Maze] = for {
    source <- Try(Source.fromFile(file))
    result <- fromLines(source.getLines().toSeq)
  } yield result
}
