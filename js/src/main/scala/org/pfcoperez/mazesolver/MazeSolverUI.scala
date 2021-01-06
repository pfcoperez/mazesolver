package org.pfcoperez.mazesolver

import org.scalajs.dom
import org.scalajs.dom.html.{Button, Canvas, Input}
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}

import org.pfcoperez.mazesolver.datastructures.Maze
import org.pfcoperez.mazesolver.model.Events._
import org.pfcoperez.mazesolver.datastructures.Maze.Claimed
import org.pfcoperez.mazesolver.datastructures.Maze.Empty
import org.pfcoperez.mazesolver.datastructures.Maze.Wall
import org.pfcoperez.mazesolver.datastructures.Maze.Cell
import org.scalajs.dom.raw.MessageEvent

object MazeSolverUI {

  def colorCode(cell: Cell): String = {
    cell match {
      case Claimed(label) => "red"
      case Wall           => "grey"
      case Empty          => "white"
    }
  }

  def renderChange(
      renderContext: Ctx2D
  )(n: Int, m: Int, i: Int, j: Int, cell: Cell): Unit = {
    val cellH = renderContext.canvas.height / n
    val cellW = cellH

    renderContext.fillStyle = colorCode(cell)
    renderContext.fillRect(j * cellW, i * cellH, cellW, cellH)
  }

  def renderMaze(renderContext: Ctx2D)(maze: Maze): Unit = {
    for {
      i <- (0 until maze.n)
      j <- (0 until maze.m)
      cell <- maze.get(i, j)
    } renderChange(renderContext)(maze.n, maze.m, i, j, cell)
  }

  def main(args: Array[String]): Unit = {

    // Main layout
    val topLevelDiv = dom.document.createElement("div")
    dom.document.body.appendChild(topLevelDiv)

    val controlPaneDiv = dom.document.createElement("div")
    topLevelDiv.appendChild(controlPaneDiv)

    // UI Elements
    val drawingCanvas =
      dom.document.createElement("canvas").asInstanceOf[Canvas]
    topLevelDiv.appendChild(drawingCanvas)
    drawingCanvas.height = (dom.window.outerHeight * 0.8).toInt
    drawingCanvas.width = (dom.window.outerWidth * 0.8).toInt
    val renderContext = drawingCanvas.getContext("2d").asInstanceOf[Ctx2D]

    val serverUrlInput = dom.document.createElement("input").asInstanceOf[Input]
    serverUrlInput.`type` = "text"
    serverUrlInput.defaultValue = "ws://localhost:8080/ws/solver"
    controlPaneDiv.appendChild(serverUrlInput)

    val connectButton =
      dom.document.createElement("button").asInstanceOf[Button]
    connectButton.textContent = "Connect"
    controlPaneDiv.appendChild(connectButton)

    val nInput =
      dom.document.createElement("input").asInstanceOf[Input]
    nInput.`type` = "number"
    nInput.width = "15"
    nInput.min = "1"
    nInput.defaultValue = "50"
    controlPaneDiv.appendChild(nInput)

    val mInput =
      dom.document.createElement("input").asInstanceOf[Input]
    mInput.`type` = "number"
    mInput.width = "15"
    mInput.min = "1"
    mInput.defaultValue = "50"
    controlPaneDiv.appendChild(mInput)

    val depthInput =
      dom.document.createElement("input").asInstanceOf[Input]
    depthInput.`type` = "number"
    depthInput.width = "15"
    depthInput.min = "0"
    depthInput.defaultValue = "70"
    controlPaneDiv.appendChild(depthInput)

    val doorsInput =
      dom.document.createElement("input").asInstanceOf[Input]
    doorsInput.`type` = "number"
    doorsInput.width = "15"
    doorsInput.min = "1"
    doorsInput.defaultValue = "2"
    controlPaneDiv.appendChild(doorsInput)

    val generateButton =
      dom.document.createElement("button").asInstanceOf[Button]
    generateButton.textContent = "Generate"
    generateButton.disabled = true
    controlPaneDiv.appendChild(generateButton)

    val solveButton =
      dom.document.createElement("button").asInstanceOf[Button]
    solveButton.textContent = "Solve"
    solveButton.disabled = true
    controlPaneDiv.appendChild(solveButton)

    val messageBox =
      dom.document.createElement("input").asInstanceOf[Input]
    messageBox.disabled = true
    controlPaneDiv.appendChild(messageBox)

    def showMessage(msg: String): Unit = {
      messageBox.value = msg
    }

    // State

    lazy val socket = new dom.WebSocket(serverUrlInput.value)
    var maze: Option[Maze] = None

    // HTML event handlers
    connectButton.onclick = { _ =>
      connectButton.disabled = true
      serverUrlInput.disabled = true

      socket.onopen = { _ =>
        showMessage("CONNECTED")
        generateButton.disabled = false
      }

      socket.onerror = { _ =>
        showMessage("ERROR: Connection error")
        generateButton.disabled = true
      }

      socket.onmessage = { (event: MessageEvent) =>
        val msgLines = event.data.toString().split("\n").toList

        msgLines match {
          case "stage" :: mazeLines =>
            Maze.fromLines(mazeLines).foreach { generated =>
              maze = Some(generated)
              renderMaze(renderContext)(maze.get)
              solveButton.disabled = false
            }
          case Event(event) :: _ =>
            event match {
              case Claim((i, j), territory) =>
                maze = maze.flatMap { maze =>
                  val cell = Claimed(territory)
                  renderChange(renderContext)(maze.n, maze.m, i, j, cell)
                  maze.update(i, j)(cell)
                }
              case f: Fusion =>
                showMessage(f.toString)
              case ExplorarionFinished =>
                showMessage("FINISHED")
            }
          case other :: _ =>
            showMessage(other)
          case _ =>
            showMessage("EMPTY RESPONSE")
        }
      }
    }

    generateButton.onclick = { _ =>
      val n = nInput.value
      val m = mInput.value
      val maxSteps = depthInput.value
      val nDoorsSide = doorsInput.value
      socket.send(s"generate $n $m $maxSteps $nDoorsSide")
    }

    solveButton.onclick = { _ =>
      maze.foreach { maze =>
        val solveCommand = s"solve\n${maze.toString}\n"
        socket.send(solveCommand)
      }
    }
  }
}