
import scala.math.sqrt

class Point2D(var X: Double = 0.0d, var Y: Double = 0.0d) {

  def apply(index: Int):Double = index match{
    case 0 => X
    case 1 => Y
    case _ => throw new ArrayIndexOutOfBoundsException()
  }

  def update(index: Int, newValue: Double):Unit = index match{
    case 0 => X = newValue
    case 1 => Y = newValue
    case _ => throw new ArrayIndexOutOfBoundsException()
  }

  def copy(p:Point2D): Unit ={
    X = p(0)
    Y = p(1)
  }

  //Computes the squared distance between an Object of Point2D and this object
  def fastSqDistance(p: Point2D): Double = {
    return  ((X - p(0)) * (X - p(0)))  + ((Y - p(1)) * (Y - p(1)))
  }

  def EuclideanDistance(p: Point2D): Double = {
    return  sqrt( ((X - p(0)) * (X - p(0)))  + ((Y - p(1)) * (Y - p(1))) )
  }

  def magnitude():Double = {
    return  sqrt((X * X) + (Y * Y))
  }

  def UnitVector():Point2D = {
    val magnitude = sqrt((X * X) + (Y * Y))
    return( new Point2D(X = X/magnitude, Y = Y/magnitude) );
  }

  def asUnitVector():Unit = {
    val magnitude = sqrt((X * X) + (Y * Y))
    X = X/magnitude
    Y = Y/magnitude
  }

  def dot(p: Point2D , p2:Point2D):Double = {
    return p(0) * p2(0) + p(1) * p2(1)
  }

  def dot(p2:Point2D):Double = {
    return X * p2(0) + Y * p2(1)
  }

  //Really basic math operations...
  def multiply(scalar: Double):Unit = {
    X *= scalar
    Y *= scalar
  }
  def multiply(p: Point2D):Unit = {
    X *= p(0)
    Y *= p(1)
  }

  def divide(scalar: Double):Unit = {
    X /= scalar
    Y /= scalar
  }
  def divide(p: Point2D):Unit = {
    X /= p(0)
    Y /= p(1)
  }

  def add(scalar: Double):Unit = {
    X += scalar
    Y += scalar
  }
  def add(p: Point2D):Unit = {
    X += p(0)
    Y += p(1)
  }
  def sub(p: Point2D):Unit = {
    X -= p(0)
    Y -= p(1)
  }
}

