import scala.swing.event.ButtonClicked
import scala.swing.{Button, Dimension, Frame, GridPanel, Label, Swing, TextArea, TextField}
//import event._

class ExternalForceFrame extends Frame{
  title = "External Force Settings:"
  preferredSize = new Dimension(400,150)
  size = new Dimension(400,150)
  peer.setLocationRelativeTo(null)// center window on screen...
  resizable = false

  //Public vars that parent class can access and pass
  var forceMagnitude = 0.0d
  var forceDuration = 30.0d
  var forceDisplacement = 15.0d

  //Style: Pulse, Damped harmonic?
  //Done button

  var MagnitudeTxt = new TextField("0.0")//Not being used yet...
  var DurationTxt = new TextField("30")
  var DisplacementTxt = new TextField("15.0")
  var DoneBtn = new Button("Save")

  contents = new GridPanel(3,2) {
    //contents += new Label("Magnitude")
    //contents += MagnitudeTxt
    contents += new Label("Duration (Frames)")
    contents += DurationTxt
    contents += new Label("Displacement (%)")
    contents += DisplacementTxt
    contents += new Label("")
    contents += DoneBtn

  }//End make new panel

  listenTo(DoneBtn)

  reactions += {
    case ButtonClicked(component) if component == DoneBtn =>
      forceMagnitude = MagnitudeTxt.text.toDouble
      forceDuration = DurationTxt.text.toDouble
      forceDisplacement = DisplacementTxt.text.toDouble
      this.visible = false
  }
}
