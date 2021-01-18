package org.pfcoperez.mazesolver

import org.pfcoperez.mazesolver.datastructures.Maze

import scala.util.Random
import scala.annotation.tailrec
import org.pfcoperez.mazesolver.datastructures.Maze.Claimed

object Generator {
  private case class AcidDroplet(
      position: (Int, Int),
      door: (Int, Int),
      ttl: Int
  ) {
    def explorationOptions(state: Maze): Vector[AcidDroplet] = {
      val (i, j) = position
      Vector(
        (i - 1, j),
        (i + 1, j),
        (i, j - 1),
        (i, j + 1)
      ).collect {
        case p @ (i, j) if ttl > 1 && state.withinMaze(i, j) =>
          AcidDroplet(p, door, ttl - 1)
      }
    }
  }

  def apply(n: Int, m: Int, trailDepth: Int, doorsPerSide: Int): Maze = {

    val east = (0 until n).map(i => (i, m - 1))
    val south = (0 until m).map(j => (n - 1, j))
    val west = (0 until n).map(i => (i, 0))
    val north = (0 until m).map(j => (0, j))

    val walls = List(east, south, west, north)
    val wallPositionsSet = walls.flatten.toSet

    val doors = walls
      .flatMap { border =>
        Random.shuffle(border.toSeq).take(doorsPerSide)
      }
      .map(pos => AcidDroplet(pos, pos, trailDepth))

    @tailrec
    def acidTrail(toExplore: List[AcidDroplet], state: Maze): Maze = {

      toExplore match {
        case (current @ AcidDroplet((i, j), _, _)) :: remaining =>
          val updatedState = state.update(i, j)(Maze.Empty).getOrElse(state)
          val candidates = current.explorationOptions(state)
          val loadedCandidates = candidates
            .filter { droplet =>
              val (i, j) = droplet.position
              !wallPositionsSet.contains(droplet.position)
            }
            .sortBy { droplet =>
              val (i, j) = droplet.position
              val (ci, cj) = droplet.door
              Math
                .sqrt(
                  (ci - i).toDouble * (ci - i).toDouble + (cj - j).toDouble * (cj - j).toDouble
                )
            } /*(implicitly[Ordering[Int]].reverse)*/
            .zipWithIndex
            .flatMap { case (candidate @ AcidDroplet((ci, cj), _, _), idx) =>
              val frequency = updatedState
                .get(ci, cj)
                .collect { case Maze.Empty =>
                  1
                }
                .getOrElse(idx + 1)
              List.fill(frequency)(candidate)
            }

          val nextToExplore = if (loadedCandidates.nonEmpty) {
            loadedCandidates(Random.nextInt(loadedCandidates.size)) :: remaining
          } else remaining

          acidTrail(nextToExplore, updatedState)
        case _ => state
      }
    }

    acidTrail(doors, Maze.fill(n, m)(Maze.Wall))
  }
}
