//This is basically an Struct object
//Holds a lot of publically accessible information

class SPHParticle {
  var pos: Point2D = new Point2D()
  var posOld: Point2D = new Point2D()//X Y
  var v: Point2D = new Point2D()//Velocity current
  var vOld: Point2D = new Point2D()//Velocity last
  var a: Point2D = new Point2D() //Acceleration

  var P: Double = 0.0d//Pressure
  var P_near: Double = 0.0d
  var D: Double = 0.0d//density
  var mass: Double = 1.0d

  def positionString():String = {
    pos(0) + "," + pos(1) //+ "," + posOld(0) + "," + posOld(1)
  }
}
