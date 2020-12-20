package org.pfcoperez.mazesolver

import org.pfcoperez.mazesolver.datastructures.Maze
import cats.collections.DisjointSets
import org.pfcoperez.mazesolver.datastructures.Maze.Cell
import cats.Order.fromOrdering
import org.pfcoperez.mazesolver.datastructures.Maze.Empty
import org.pfcoperez.mazesolver.datastructures.Maze.Wall
import org.pfcoperez.mazesolver.datastructures.Maze.Claimed

object Solver {

  case class Solution(claimed: Maze, territories: DisjointSets[Int])
  case class Scout(position: (Int, Int), territory: Int) {
    assert(territory >= 0)
  }
  case class StepResult(
      territories: DisjointSets[Int],
      stage: Maze,
      toExplore: List[Scout]
  ) {
    def explorationFinished: Boolean = toExplore.isEmpty
  }

  sealed trait Event
  case class Claim(position: (Int, Int), territory: Int) extends Event
  case class Fusion(territoryA: Int, territoryB: Int) extends Event
  case object ExplorarionFinished extends Event

  def explorationStep(
      previousState: StepResult
  ): (StepResult, Option[Event]) = {
    import previousState._

    toExplore match {
      case Nil => previousState -> None
      case Scout(scoutPosition @ (i, j), territory) :: remaining =>
        val maybeEvent = stage.get(i, j).collect {
          case Empty => Claim(scoutPosition, territory)
          case Claimed(ownerTerritory) =>
            Fusion(ownerTerritory, territory)
        }

        val updatedTerritories = maybeEvent
          .collect { case Fusion(ownerTerritory, territory) =>
            territories.union(ownerTerritory, territory)._1
          }
          .getOrElse(territories)

        val updatedStage = maybeEvent
          .flatMap {
            case Claim((i, j), territory) =>
              stage.update(i, j)(Maze.Claimed(territory))
            case _ => None
          }
          .getOrElse(stage)

        previousState.copy(
          territories = updatedTerritories,
          stage = updatedStage,
          toExplore =
            remaining ::: List((i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j))
              .map(Scout(_, territory))
        ) -> maybeEvent
    }
  }

  def initialConditions(input: Maze): StepResult = {
    implicit val intOrder = fromOrdering[Int]

    def isPosition(value: Maze.Cell) =
      (input.get _).tupled.andThen(_.exists(_ == value))

    val positionToDoor = {
      val isEmptyPosition = isPosition(Maze.Empty)

      val eastDoors =
        (0 until input.n).map(i => (i, input.m - 1)).filter(isEmptyPosition)
      val southDoors =
        (0 until input.m).map(j => (input.n - 1, j)).filter(isEmptyPosition)
      val westDoors =
        (0 until input.n).map(i => (i, 0)).filter(isEmptyPosition)
      val northDoors =
        (0 until input.m).map(j => (0, j)).filter(isEmptyPosition)

      (eastDoors ++ southDoors ++ westDoors ++ northDoors).zipWithIndex.toMap
    }

    val initialtTerritories = DisjointSets(positionToDoor.values.toSeq: _*)

    StepResult(
      initialtTerritories,
      positionToDoor.foldLeft(input) { case (maze, ((i, j), door)) =>
        maze.update(i, j)(Claimed(door)).getOrElse(maze)
      },
      positionToDoor.view.toList.map { case (position, door) =>
        Scout(position, door)
      }
    )
  }

}
