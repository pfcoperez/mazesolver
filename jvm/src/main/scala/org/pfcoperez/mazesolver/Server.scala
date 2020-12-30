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
import org.pfcoperez.mazesolver.Solver.Event

object Server extends App {
  import Server.WSEntities._

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
            requestLines.mkString("\n") match {
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
            // println(s"Solving:\n$problem")
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
    def solutionStep(state: StepResult): Option[(StepResult, Event)] = {
      val (updatedState, maybeEvent) = Solver.explorationStep(state)
      // println(updatedState.toExplore.map(_.position))
      maybeEvent match {
        case Some(event) => Some(updatedState -> event)
        case None if updatedState.toExplore.nonEmpty =>
          solutionStep(updatedState)
        case _ => None
      }
    }
    val solutionsStream =
      Source.unfold[StepResult, Event](Solver.initialConditions(maze))(
        solutionStep
      )
    problem.maybeMaxIterations
      .map(maxIterations => solutionsStream.take(maxIterations.toLong))
      .getOrElse(solutionsStream)
  }

  val bindingFuture: Future[ServerBinding] =
    Http().bindAndHandle(route, "localhost", 8080)

  object WSEntities {
    sealed trait Request
    object Request {

      private val GenerateRegex =
        "generate ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)".r
      private val SolveRegex =
        """solve(\s[0-9]+)?""".r

      def unapply(rawRequest: String): Option[Request] = {
        Some(rawRequest).flatMap {
          case "noop" => Some(NoOp)
          case GenerateRegex(nStr, mStr, doorsStr, depthStr) =>
            Some(
              Generate(nStr.toInt, mStr.toInt, doorsStr.toInt, depthStr.toInt)
            )
          case solveStr: String =>
            val tokenized = solveStr.split("\n")
            tokenized.headOption.flatMap { case SolveRegex(maybeMaxItsStr) =>
              val maybeMaxIterations = for {
                str <- Option(maybeMaxItsStr)
                limit <- Try(str.trim.toInt).toOption
              } yield limit
              tokenized.tail.headOption.map { _ =>
                Solve(tokenized.tail.mkString("\n"), maybeMaxIterations)
              }
            }
        }
      }
    }

    case object NoOp extends Request
    case class Generate(n: Int, m: Int, doorsPerSide: Int, depth: Int)
        extends Request

    case class Solve(
        maze: String,
        maybeMaxIterations: Option[Int]
    ) extends Request

    sealed trait Response

    case object InvalidRequest extends Response with Request
    case object Ack extends Response
    case class Stage(maze: Maze) extends Response {
      override def toString: String = s"stage\n$maze"
    }
    case class SolutionEvent(event: Event) extends Response {
      override def toString: String = event.toString
    }
  }
}
