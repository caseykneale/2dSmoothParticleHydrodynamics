import scala.collection.JavaConverters._
import scala.swing.event.MouseClicked
//Imports for UI
import scala.swing.{BorderPanel, Dimension, Frame, Graphics2D, Panel, Rectangle, Swing}
import java.awt.Color



//This is a small extension of a typical frame...
class SimulationFrame extends Frame {
  //Constructor for UI elements
  protected val winW:Int = 503 //Window Width
  protected val winH:Int = 540 //Window Height

  var pointWidth = 0
  var pointHeight = 0

  title = "Smooth Particle Hydrodynamics Simulation"
  preferredSize = new Dimension(winW,winH)
  size = new Dimension(winW,winH)
  peer.setLocationRelativeTo(null)// center window on screen...
  resizable = false

  var pListBufferConc = new java.util.concurrent.ConcurrentHashMap[Int,(Int,Int)]().asScala

  var TriggerExternalForce = false

  background = Color.WHITE

  //var SPHSim = new Simulation()
  //val simthread = new Thread(SPHSim)

  contents = new Panel {
    listenTo(mouse.clicks)

    reactions += {
      case e: MouseClicked =>
        TriggerExternalForce = true
    }

    background = Color.WHITE
    foreground = Color.BLUE

    override def paint(g: Graphics2D) {
      super.paint(g)

      g.drawRect(0, 0, 500, 500)
      foreground = Color.BLUE

      var p: Int = 0 //reset particle iterator
      val particlecount = pListBufferConc.size
      while (p < particlecount) {
        val (x:Int,y:Int) = pListBufferConc(p)
        g.drawOval(x , y, pointWidth, pointHeight)
        p += 1
      } //end particle loop

    }//End override paint method

  }//End make new panel




  def requestRepaint():Unit={
    repaint()
  }



}
