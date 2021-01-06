package org.pfcoperez.mazesolver.model

import org.pfcoperez.mazesolver.model.Events._
import org.pfcoperez.mazesolver.datastructures.Maze

import scala.util.Try

object Protocol {
  sealed trait Request
  object Request {

    private val NoOpRegex = "noop\n*".r
    private val GenerateRegex =
      "generate ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)\n*".r
    private val SolveRegex =
      """solve(\s[0-9]+)?""".r

    def unapply(rawRequest: String): Option[Request] = {
      Some(rawRequest).flatMap {
        case NoOpRegex() => Some(NoOp)
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
