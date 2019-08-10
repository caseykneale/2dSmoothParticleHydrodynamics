import UIWorkbench.startSimBtn

import scala.swing.event.ButtonClicked
import scala.swing.{Button, Dimension, FileChooser, FlowPanel, Frame, ScrollPane, Swing}
import java.io.File

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.math.pow

class ViewerFrame extends Frame{
  title = "Simulation Viewer:"
  preferredSize = new Dimension(503,600)
  size = new Dimension(503,600)
  peer.setLocationRelativeTo(null)// center window on screen...
  resizable = false
  //file I/O stuff
  var fileDialog:FileChooser = new FileChooser()
  var simulationFile:File = new File("")
  var settingFile:File = new File("")
  var FileBuffer:BufferedSource = null
  var FileLines:Iterator[String] = null
  var BufferOpen = true
  //Simulation Stuff
  val ONE_FRAME: Int = 17
  var particles: Int = 2000
  protected val winW:Int = 500 //Drawing Window Width
  protected val winH:Int = 500 //Drawing Window Height
  private var constraint = new Box2D(new Point2D(0.0, 10.0), new Point2D(10.0, 0.0))
  var constraintW = constraint.LowerRight(0) - constraint.UpperLeft(0)
  var constraintH = constraint.UpperLeft(1) - constraint.LowerRight(1)
  var drawScaleX = (winW.toDouble) / constraintW
  var drawScaleY = (winH.toDouble) / constraintH
  protected val pi: Double = 3.14159265359d //Numerical constant
  private var VOLUME: Double = 2.00d / 1.0d //Total volume of fluid
  val defaultsVars = mutable.LinkedHashMap[String,Double]("Particles" -> 2000, "Neighbors"-> 32,
    "Liquid Volume"-> 2.0, "Container Volume" -> 10.0)
  var PDIST: Double = pow((VOLUME * 0.5d) / (pi * defaultsVars("Particles")), 0.5d).toDouble //Kernel support radius
  var pointWidth = (PDIST * 0.50d * drawScaleX).toInt
  var pointHeight = (PDIST * 0.50d * drawScaleY).toInt
  //UI stuff
  var openSettingBtn = new Button("Load Settings")
  var openSimulationBtn = new Button("Load Simulation")
  var startBtn = new Button("Start")
  var stopBtn = new Button("Stop")
  stopBtn.enabled = false


  //

  val scroll = new ViewerPanel()
  scroll.preferredSize = new Dimension(503,500)
  openSettingBtn.preferredSize = new Dimension(160,50)
  openSimulationBtn.preferredSize = new Dimension(160,50)
  startBtn.preferredSize = new Dimension(72,50)
  stopBtn.preferredSize = new Dimension(72,50)

  contents = new FlowPanel {
    contents += scroll
    contents += openSettingBtn
    contents += openSimulationBtn
    contents += startBtn
    contents += stopBtn
  }//End make new panel

  listenTo(openSettingBtn)
  listenTo(openSimulationBtn)
  listenTo(startBtn)
  listenTo(stopBtn)

  reactions += {
    case ButtonClicked(component) if component == openSettingBtn =>
      var returnVal = fileDialog.showOpenDialog(null)
      if (returnVal == FileChooser.Result.Approve) {
        settingFile = fileDialog.selectedFile
        val listRep = defaultsVars.keysIterator.toList
        for (line <- Source.fromFile(settingFile).getLines) {
          val partition:Int = line.indexOfSlice(":")
          val label: String = line.substring(0, partition)
          val value: String = line.substring(partition+1, line.length)
          //if key exists then store the value to the map
          if(defaultsVars.contains(label) == true) {
            defaultsVars(label) = value.toDouble
          }
          PDIST = pow((VOLUME * 0.5d) / (pi * defaultsVars("Particles")), 0.5d).toDouble
        }

      }

    case ButtonClicked(component) if component == openSimulationBtn =>
      var returnVal = fileDialog.showOpenDialog(null)
      if (returnVal == FileChooser.Result.Approve) {
        simulationFile = fileDialog.selectedFile
        scroll.pointHeight = pointHeight
        scroll.pointWidth = pointWidth
      }
    case ButtonClicked(component) if component == startBtn =>
      startBtn.enabled = false
      stopBtn.enabled = true
      FileBuffer = Source.fromFile(simulationFile)
      FileLines = FileBuffer.getLines()
      BufferOpen = true
      //Initialize particle position list with zeros...
      for (i <- 0 to defaultsVars("Particles").toInt - 1) {
        scroll.pListViewBuffer.put(i, (0, 0))
      }
      timer.start()

    case ButtonClicked(component) if component == stopBtn =>
      BufferOpen = false
      startBtn.enabled = true
      stopBtn.enabled = false
      timer.stop()
  }

  val timer = new javax.swing.Timer(ONE_FRAME, Swing.ActionListener(e => {
    if(BufferOpen == true) {
      if (FileLines.hasNext == true) {
        for (i <- 0 to defaultsVars("Particles").toInt - 1) {
          val line = FileLines.next
          val comma: Int = line.indexOfSlice(",")
          val X: String = line.substring(0, comma)
          val Y: String = line.substring(comma + 1, line.length)
          scroll.pListViewBuffer.replace(i, ((X.toDouble * drawScaleX).toInt, winH - (Y.toDouble * drawScaleY).toInt))
        }
      } else {
        BufferOpen = false
        FileBuffer.close()
        startBtn.enabled = true
        stopBtn.enabled = false
      }
    }

    repaint()
    //Thread.sleep(100)
  }))



}
