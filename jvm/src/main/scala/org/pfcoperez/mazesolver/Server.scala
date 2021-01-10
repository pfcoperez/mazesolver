package org.pfcoperez.mazesolver

import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.stream.scaladsl._

import scala.concurrent.Future
import scala.util.{Try, Success}

import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.TextMessage

import akka.stream.scaladsl.Sink

import akka.stream.scaladsl.Source
import akka.NotUsed
import org.pfcoperez.mazesolver.datastructures.Maze
import org.pfcoperez.mazesolver.Solver.StepResult
import org.pfcoperez.mazesolver.model.Events._
import org.pfcoperez.mazesolver.model.Protocol._
import cats.collections.DisjointSets

import cats.Order.fromOrdering

object Server extends App {

  implicit val system = ActorSystem("server-system")

  val route = path("generate") {
    parameter(
      'n.as[Int],
      'm.as[Int],
      'doors_side.as[Int],
      'trail_depth.as[Int].?
    ) { (n, m, doorsPerSide, maybeDepth) =>
      val depth = maybeDepth.getOrElse(Math.sqrt(n * n + m * m).toInt)
      complete {
        Generator(n, m, depth, doorsPerSide).toString
      }
    }
  } ~ pathPrefix("ws") {
    path("solver") {
      handleWebSocketMessages(wsServerFlow)
    }
  }

  def wsServerFlow: Flow[Message, Message, Any] = {
    Flow[Message].flatMapConcat {
      case textMessage: TextMessage =>
        val requestStream: Source[Request, _] = textMessage.textStream
          .fold(Vector.empty[String]) { case (lines, line) =>
            lines :+ line
          }
          .map { requestLines =>
            requestLines.mkString("") match {
              case Request(rq) => rq
              case _ =>
                System.err.println(
                  s"Invalid request:\n>${requestLines.mkString("\n")}<"
                )
                InvalidRequest
            }
          }

        val responseStream: Source[Response, _] = requestStream.flatMapConcat {
          case NoOp => Source.single[Response](Ack)
          case Generate(n, m, doors, depth) =>
            Source.single[Response](Stage(Generator(n, m, doors, depth)))
          case problem: Solve =>
            //println(s"Solving:\n>$problem<")
            solutionsStream(problem).map(SolutionEvent.apply)

          case _ => Source.single[Response](InvalidRequest)
        }

        responseStream.map(response => TextMessage(s"$response\n"))

      case binaryMessage: BinaryMessage =>
        binaryMessage.dataStream.runWith(Sink.ignore)
        Source.single(TextMessage(InvalidRequest.toString))
    }
  }

  def solutionsStream(problem: Solve): Source[Event, _] = {
    val Success(maze) = Maze.fromLines(problem.maze.split("\n")) //TODO: Unsafe
    def solutionStep(
        state: StepResult
    ): Option[(StepResult, (Event, DisjointSets[Int]))] = {
      val (updatedState, maybeEvent) = Solver.explorationStep(state)
      // println(updatedState.toExplore.map(_.position))
      maybeEvent match {
        case Some(event) =>
          Some(updatedState -> (event, updatedState.territories))
        case None if updatedState.toExplore.nonEmpty =>
          solutionStep(updatedState)
        case _ => None
      }
    }

    val initialConditions = Solver.initialConditions(maze)

    implicit val pairsOrder = fromOrdering[(Int, Int)]

    val solutionsStream =
      Source.unfold[StepResult, (Event, DisjointSets[Int])](
        initialConditions
      )(
        solutionStep
      )
    problem.maybeMaxIterations
      .map(maxIterations => solutionsStream.take(maxIterations.toLong))
      .getOrElse(solutionsStream)
      .mapConcat { entry =>
        List(
          Right[DisjointSets[Int], Event](entry._1),
          Left[DisjointSets[Int], Event](entry._2)
        )
      }
      .prepend(
        Source.single(
          Left[DisjointSets[Int], Event](initialConditions.territories)
        )
      )
      .concat(
        Source.single(
          Right[DisjointSets[Int], Event](ExplorationFinished(List.empty))
        )
      )
      .grouped(2)
      .map {
        case Seq(
              Left(territories),
              Right(finalization: ExplorationFinished)
            ) =>
          val equivalences: List[(Int, Int)] = for {
            (parent, equivalences) <- territories.toSets._2.toList
            territory <- equivalences.toList
          } yield territory -> parent

          ExplorationFinished(equivalences)
        case Seq(Left(_), Right(event)) =>
          event
      }
  }
  val bindingFuture: Future[ServerBinding] =
    Http().bindAndHandle(route, "localhost", 8080)
}
