class Box2D(val UpperLeft: Point2D, val LowerRight: Point2D) {

  def isPointInside(p: Point2D):Boolean = {
    var condition = false//Default is false, prove it wrong, or don't!
    if(p(0) >= UpperLeft(0) && p(0) <= LowerRight(0) ){//Inside X bounds?
      if(p(1) <= UpperLeft(1) && p(1) >= LowerRight(1) ){//Inside Y bounds?
        condition = true
      }
    }
    return condition
  }

}