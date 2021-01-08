package org.pfcoperez.mazesolver

import org.pfcoperez.mazesolver.datastructures.Maze
import cats.collections.DisjointSets
import org.pfcoperez.mazesolver.datastructures.Maze.Cell
import cats.Order.fromOrdering
import org.pfcoperez.mazesolver.datastructures.Maze.Empty
import org.pfcoperez.mazesolver.datastructures.Maze.Wall
import org.pfcoperez.mazesolver.datastructures.Maze.Claimed
import org.pfcoperez.mazesolver.model.Events._

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

  def explorationStep(
      previousState: StepResult
  ): (StepResult, Option[Event]) = {
    import previousState._

    toExplore match {
      case Nil => previousState -> None
      case Scout(scoutPosition @ (i, j), territory) :: remaining =>
        val maybeEvent = stage.get(i, j).collect {
          case Empty =>
            Claim(scoutPosition, territory)
          case Claimed(ownerTerritory)
              if territories
                .find(ownerTerritory)
                ._2 != territories.find(territory)._2 =>
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

        def worthOfExploring(position: (Int, Int), cell: Cell): Boolean = {
          cell match {
            case Empty => true
            case Claimed(otherTerritory) =>
              (otherTerritory != territory) && (Math.abs(i - position._1) + Math
                .abs(j - position._2) < 2)
            case _ => false
          }
        }

        val nextToExplore = maybeEvent.map { _ =>
          List((i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j))
            .collect {
              case pos @ (i, j)
                  if stage.get(i, j).exists(worthOfExploring(pos, _)) =>
                Scout(pos, territory)
            }
        } getOrElse Nil

        previousState.copy(
          territories = updatedTerritories,
          stage = updatedStage,
          toExplore = remaining ::: nextToExplore
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
        (0 until (input.n - 1))
          .map(i => (i, input.m - 1))
          .filter(isEmptyPosition)
      val southDoors =
        (1 until input.m).map(j => (input.n - 1, j)).filter(isEmptyPosition)
      val westDoors =
        (1 until input.n).map(i => (i, 0)).filter(isEmptyPosition)
      val northDoors =
        (0 until (input.m - 1)).map(j => (0, j)).filter(isEmptyPosition)

      (eastDoors ++ southDoors ++ westDoors ++ northDoors).zipWithIndex.toMap
    }

    val initialtTerritories = DisjointSets(positionToDoor.values.toSeq: _*)

    StepResult(
      initialtTerritories,
      /*positionToDoor.foldLeft(input) { case (maze, ((i, j), door)) =>
        maze.update(i, j)(Claimed(door)).getOrElse(maze)
      }*/
      input,
      positionToDoor.view.toList.map { case (position, door) =>
        Scout(position, door)
      }
    )
  }

}
