import java.awt.Color
import scala.collection.JavaConverters._
import scala.swing.{BorderPanel, Dimension, Frame, Graphics2D, Panel, Rectangle, Swing}

class ViewerPanel extends Panel {
  background = Color.WHITE
  foreground = Color.BLUE

  var pListViewBuffer = new java.util.concurrent.ConcurrentHashMap[Int,(Int,Int)]().asScala
  var pointWidth = 0
  var pointHeight = 0

  override def paint(g: Graphics2D) {
    super.paint(g)
    g.drawRect(0, 0, 500, 500)
    foreground = Color.BLUE

    var p: Int = 0 //reset particle iterator
    val particlecount = pListViewBuffer.size
    while (p < particlecount) {
      val (x:Int,y:Int) = pListViewBuffer(p)
      g.drawOval(x , y, pointWidth, pointHeight)
      p += 1
    } //end particle loop

  }//End override paint method

}
