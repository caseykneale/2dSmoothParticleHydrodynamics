import scala.swing.{Dimension, _}
import event._
import scala.collection.mutable
import scala.swing.SplitPane
import scala.swing.GridPanel
import java.io.{File, PrintWriter}

import scala.io.Source

object UIWorkbench extends SimpleSwingApplication{
  val ONE_FRAME: Int = 17
  var simWindow:SimulationFrame = new SimulationFrame()
  var externalForceWindow:ExternalForceFrame = new ExternalForceFrame()
  var viewerWindow:ViewerFrame = new ViewerFrame()
  var fileDialog:FileChooser = new FileChooser()
  var logFile: File = new File("")

  var startSimBtn = new Button("Start")
  var logSim = new CheckBox("Log Simulation")
  var sample = new CheckBox("Randomly Sample")
  var neighbors = new CheckBox("Store Neighbors")
  sample.selected = true
  neighbors.selected = true

  val colInit = new RadioMenuItem("Column Initialize")
  val poolInit = new RadioMenuItem("Pool Initialize")

  var SPHSim = new Simulation()


  simWindow.pListBufferConc = SPHSim.pListBufferConc

  var simthread = new Thread(SPHSim)

  val defaultsVars = mutable.LinkedHashMap[String,Double]("Particles" -> 2000, "Neighbors"-> 32,
    "Duration" -> 3000, "Time Step" -> 0.003,
    "Liquid Volume"-> 2.0, "Container Volume" -> 10.0, "Gravity" -> -400, "Viscosity Sigma"-> 0.01, "Viscosity Beta" -> 0.002,
    "Elastic Stiffness" -> 10.0, "Elastic Near" -> 500.0)

  val textboxes = Array.ofDim[TextField](n1 = defaultsVars.size)
  val labels = Array.ofDim[Label](n1 = defaultsVars.size)

  for (((lbl, vals),index) <- defaultsVars.zipWithIndex) {
    labels(index) = new Label(lbl.toString())
    textboxes(index) = new TextField(vals.toString())
  }


  def top = new MainFrame(){
    title = "Toolbox"
    preferredSize = new Dimension(300,500)
    resizable = false//Toolbox must be one size...


    menuBar = new MenuBar { menuModal =>
      contents += new Menu("File") {

        contents += new MenuItem(Action("Save Settings"){
          var returnVal = fileDialog.showSaveDialog( menuModal )
          if (returnVal == FileChooser.Result.Approve) {
            val file = fileDialog.selectedFile
            println("Saving: " + file.getName() + ".")
            val parameterMap = collection.mutable.Map[String, Double]()

            for(param <- 0 to (defaultsVars.size - 1)){
              parameterMap += labels(param).text -> textboxes(param).text.toDouble
            }

            new PrintWriter(file) {
              parameterMap.foreach {
                case (k, v) =>
                  println(k + ":" + v)
              }
              close()
            }
          }
        })

        contents += new MenuItem(Action("Load Settings") {
          var returnVal = fileDialog.showOpenDialog( menuModal )
          if (returnVal == FileChooser.Result.Approve) {
            val file = fileDialog.selectedFile
            println("Opening: " + file.getName() + ".")
            val listRep = defaultsVars.keysIterator.toList
            for (line <- Source.fromFile(file).getLines) {
              val partition:Int = line.indexOfSlice(":")
              val label: String = line.substring(0, partition)
              val value: String = line.substring(partition+1, line.length)
              textboxes(listRep.indexOf(label)).text = value
            }

          }
        })
        contents += new Separator
        contents += new MenuItem(Action("Log Simulation"){
          var returnVal = fileDialog.showSaveDialog( menuModal )
          if (returnVal == FileChooser.Result.Approve) {
            logFile = fileDialog.selectedFile
          }
        })

        contents += new Separator
        contents += new MenuItem(Action("Exit"){
          System.exit(0)
        })
      }
      contents += new Menu("Simulation") {
        colInit.selected = true
        val mutex = new ButtonGroup(colInit, poolInit)
        contents ++= mutex.buttons
        contents += new Separator
        contents += new MenuItem(Action("External Force"){
          externalForceWindow.visible = true
        })

      }
      contents += new Menu("Viewer") {
        contents += new MenuItem(Action("Launch"){
          viewerWindow.pointHeight = SPHSim.pointHeight
          viewerWindow.pointWidth = SPHSim.pointWidth
          viewerWindow.visible = true
        })

      }
    }


    startSimBtn = new Button("Start")
    val stopSimBtn = new Button("Stop")
    stopSimBtn.enabled = false
    val startstoppane = new SplitPane(Orientation.Vertical, startSimBtn,stopSimBtn)


    //startstoppane.resizeWeight = 0.5
    contents = new GridPanel(defaultsVars.size + 2,2) {
      //var index
      for( index <- 0 to (defaultsVars.size - 1)) {//Gotta be a cleaner way to get the range...
        contents += labels(index)
        contents += textboxes(index)
      }
      contents += logSim
      contents += sample
      contents += neighbors
      contents += startstoppane

    }

    // specify which Components produce events of interest
    listenTo(startSimBtn)
    listenTo(stopSimBtn)

    // react to events
    reactions += {
      case ButtonClicked(component) if component == startSimBtn =>

        UpdateSimulationParameters()

        SPHSim.logState(logSim.selected)
        SPHSim.StoreNeighbors(neighbors.selected)
        SPHSim.RandomSample(sample.selected)

        SPHSim.setLogFile(logFile)
        logSim.enabled = false
        sample.enabled = false
        neighbors.enabled = false

        simWindow.visible = true
        Simulate()
        startSimBtn.enabled = false
        stopSimBtn.enabled = true

      case ButtonClicked(component) if component == stopSimBtn =>
        EndSimulation()
        //simWindow.visible = false
        startSimBtn.enabled = true
        stopSimBtn.enabled = false
        logSim.enabled = true
        sample.enabled = true
        neighbors.enabled = true
    }

  }

  def UpdateSimulationParameters():Unit = {
    val parameterMap = collection.mutable.Map[String, Double]()

    for(param <- 0 to (defaultsVars.size - 1)){
      parameterMap += labels(param).text -> textboxes(param).text.toDouble
    }

    //Pass external force values...
    SPHSim.forceDisplacement = externalForceWindow.forceDisplacement
    SPHSim.forceMagnitude = externalForceWindow.forceMagnitude
    SPHSim.forceDuration = externalForceWindow.forceDuration

    if(colInit.selected == true){
      SPHSim.InitializationType = "COLUMN"
    }else{
      SPHSim.InitializationType = "POOL"
    }

    SPHSim.updateSimulationParameters(parameterMap)

  }

  def Simulate():Unit = {
    SPHSim.PauseSimulationLoop(false)
    simWindow.pointHeight = SPHSim.pointHeight
    simWindow.pointWidth = SPHSim.pointWidth
    simthread.setPriority(9)
    if(simthread.isAlive == false) {
      simthread.start()
    }
    timer.start()
  }

  def EndSimulation():Unit = {
    SPHSim.PauseSimulationLoop(true)
    timer.stop()//Set window back to default drawing mode
    simthread.interrupt()//Be polite and try to stop the thread...
    simthread = new Thread(SPHSim)//Be agressive and stop current thread...
  }


  val timer = new javax.swing.Timer(ONE_FRAME, Swing.ActionListener(e => {
    simWindow.pListBufferConc = SPHSim.pListBufferConc
    simWindow.requestRepaint()
    //This handles passing the mouse click event from the frame to the simulation...
    if(simWindow.TriggerExternalForce == true){
      simWindow.TriggerExternalForce = false
      SPHSim.ExternalForceTrigger = true
    }
  }))



}
