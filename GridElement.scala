import collection.mutable.ArrayBuffer

class GridElement(val UpperLeftGrid: Point2D, val LowerRightGrid: Point2D)
  extends Box2D(UpperLeftGrid, LowerRightGrid) {

  val ParticlesInside = new ArrayBuffer[Int]()

  def addParticleIndex(ind: Int){
    ParticlesInside += ind
  }

  def removeParticleIndex(ind: Int){
    val indloc:Int = ParticlesInside.indexOf(ind) //find unique particles identifier in list
    if(indloc >= 0){
      ParticlesInside.remove(indloc)
    }
  }

  def IsInside(index: Int):Boolean = {
    return ParticlesInside.contains(index)
  }

  def AddIfInside(pnt: Point2D, index: Int){
    if(super.isPointInside(pnt) == true){
      ParticlesInside += index
    }
  }

  /** This function is kind of a bad idea to begin with, with Scala though, not needed...
  def getList() :ArrayBuffer[Int] = {
    return ParticlesInside
  }**/

}

/**
import java.util.ArrayList;

public class GridElement extends Box2D {

  private ArrayList<Integer> ParticlesInside = new ArrayList<Integer>();

  public GridElement (Point2D UpLft, Point2D LowRgt){
    super(UpLft, LowRgt);
  }







}**/