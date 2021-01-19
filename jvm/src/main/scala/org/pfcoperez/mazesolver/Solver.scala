package org.pfcoperez.mazesolver

import org.pfcoperez.mazesolver.datastructures.Maze
import org.pfcoperez.mazesolver.datastructures.Maze.Cell
import org.pfcoperez.mazesolver.datastructures.Maze.Empty
import org.pfcoperez.mazesolver.datastructures.Maze.Wall
import org.pfcoperez.mazesolver.datastructures.Maze.Claimed
import org.pfcoperez.mazesolver.model.Events._
import org.pfcoperez.mazesolver.datastructures.DisjointSetsWrapper

object Solver {

  case class Solution(
      claimed: Maze,
      territories: DisjointSetsWrapper[Int]
  )
  case class Scout(position: (Int, Int), territory: Int) {
    assert(territory >= 0)
  }
  case class StepResult(
      territories: DisjointSetsWrapper[Int],
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
        val (maybeEvent, optimizedTerritories) = stage
          .get(i, j)
          .collect {
            case Empty =>
              Some(Claim(scoutPosition, territory)) -> territories
            case Claimed(ownerTerritory) =>
              val (optimizedOnce, ownerParent) =
                territories.find(ownerTerritory)
              val (optimizedTwice, territoryParent) =
                optimizedOnce.find(territory)

              val result = if (ownerParent != territoryParent) {
                Some(Fusion(ownerTerritory, territory))
              } else None
              result -> optimizedTwice
          }
          .getOrElse(None -> territories)

        val updatedTerritories = maybeEvent
          .collect { case Fusion(ownerTerritory, territory) =>
            optimizedTerritories.union(ownerTerritory, territory)._1
          }
          .getOrElse(optimizedTerritories)

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

  def initialConditions(
      input: Maze,
      simulatedTerritorySize: Int
  )(
      disjointSetsFactory: Seq[Int] => DisjointSetsWrapper[Int]
  ): StepResult = {

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
        .mapValues(_ * simulatedTerritorySize)
    }

    val initialTerritories = positionToDoor.foldLeft(
      disjointSetsFactory(
        positionToDoor.values.toSeq.flatMap(realDoor =>
          realDoor until (realDoor + simulatedTerritorySize)
        )
      )
    ) { case (territories, ((i, j), door)) =>
      ((door + 1) until (door + simulatedTerritorySize)).foldLeft(
        territories
      ) { case (territories, k) =>
        territories.union(door, k)._1
      }
    }

    StepResult(
      initialTerritories,
      input,
      positionToDoor.view.toList.map { case (position, door) =>
        Scout(position, door)
      }
    )
  }

}
