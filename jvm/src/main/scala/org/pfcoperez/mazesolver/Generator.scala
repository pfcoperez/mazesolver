package org.pfcoperez.mazesolver

import org.pfcoperez.mazesolver.datastructures.Maze

import scala.util.Random
import scala.annotation.tailrec

object Generator {
  private case class AcidDroplet(position: (Int, Int), ttl: Int) {
    def explorationOptions(state: Maze): Vector[AcidDroplet] = {
      val (i, j) = position
      Vector(
        (i - 1, j),
        (i + 1, j),
        (i, j - 1),
        (i, j + 1)
      ).collect {
        case p @ (i, j) if ttl > 1 && state.withinMaze(i, j) =>
          AcidDroplet(p, ttl - 1)
      }
    }
  }

  def apply(n: Int, m: Int, trailDepth: Int, doorsPerSide: Int): Maze = {

    val east = (0 until n).map(i => (i, m - 1))
    val south = (0 until m).map(j => (n - 1, j))
    val west = (0 until n).map(i => (i, 0))
    val north = (0 until m).map(j => (0, j))

    val doors = List(east, south, west, north)
      .flatMap { border =>
        Random.shuffle(border.toSeq).take(doorsPerSide)
      }
      .map(AcidDroplet(_, trailDepth))

    @tailrec
    def acidTrail(toExplore: List[AcidDroplet], state: Maze): Maze = {

      toExplore match {
        case (current @ AcidDroplet((i, j), _)) :: remaining =>
          val updatedState = state.update(i, j)(Maze.Empty).getOrElse(state)
          val candidates = current.explorationOptions(state)
          val loadedCandidates = candidates
            .sortBy { droplet =>
              val (i, j) = droplet.position
              val (ci, cj) = (state.n / 2, state.m / 2)
              (ci - i) * (ci - i) + (cj - j) * (cj - j)
            }(implicitly[Ordering[Int]].reverse)
            .zipWithIndex
            .flatMap { case (candidate, idx) =>
              List.fill(idx + 1)(candidate)
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
