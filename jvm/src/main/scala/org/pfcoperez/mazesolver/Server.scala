package org.pfcoperez.mazesolver

import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._

import scala.concurrent.Future

import akka.http.scaladsl.model.ws.Message
import akka.http.scaladsl.model.ws.BinaryMessage
import akka.http.scaladsl.model.ws.TextMessage

import akka.stream.scaladsl.Sink

import org.pfcoperez.mazesolver.Server.WSEntities.InvalidRequest
import akka.stream.javadsl.Source

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
      complete("OK")
    }
  }

  /*def solverFlow: Flow[Message, Message, Any] = {
    val invalidRequest = TextMessage(InvalidRequest.toString)
    Flow[Message] {
      case textMessage: TextMessage =>
        val responseF =
          textMessage.textStream.runWith(Sink.fold(Vector.empty[String]) {
            case (lines, line) => lines.append(line)
          }).map {
            case Vector("noop") =>
            case _ => ???
          }

        val responseStream = Source.future(responseF).ru

        TextMessage(responseStream)

      case binaryMessage: BinaryMessage =>
        binaryMessage.runWith(Sink.ignore)
        invalidRequest
    }
  }*/

  val bindingFuture: Future[ServerBinding] =
    Http().bindAndHandle(route, "localhost", 8080)

  object WSEntities {
    sealed trait Request
    object Request {
      def unapply(rawRequest: String): Option[Request] = {
        Some(rawRequest).collect { case "noop" =>
          NoOp
        }
      }
    }

    case object NoOp extends Request

    case class Solve(
        maze: String,
        maybeMaxIterations: Option[Int]
    ) extends Request

    sealed trait Response

    case object InvalidRequest extends Response
    case object Ack extends Response
  }
}
