import java.io.{File, FileWriter}

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

//Imports for Mathematics
import scala.math.sqrt
import scala.math.pow
import scala.math.floor
import scala.math.abs

import java.io.PrintWriter
import scala.io.Source

class Simulation extends Runnable{
  protected val winW:Int = 500 //Window Width
  protected val winH:Int = 500 //Window Height

  protected val pi: Double = 3.14159265359d //Numerical constant

  //Simulation constants
  private var Particle_Count:Int = 2000 //Number of particles in simulation
  private var avKernel:Int = 32 //Number of nearest neighbors to calculate
  private var distArr = Array.ofDim[Double](n1 = avKernel) //Vector of particle distances
  private var indexArr = Array.ofDim[Int](n1 = avKernel) //Vector of particle distances

  private val skip_frames:Int = 1 //Number of frames to skip for rendering
  private var dt: Double = 0.003 //Simulation time step
  private val ti: Double = 0.0d
  private var t_f: Double = 3000.0d
  var InitializationType = "COLUMN" //Can be COLUMN or POOL

  //Particle variables
  private var gravity: Double = -400.0d			//Gravitational force on particles
  private var VOLUME: Double = 2.00d / 1.0d //Total volume of fluid
  private var VISCOSITY_SIGMA: Double = 0.01d //Linear viscosity term
  private var VISCOSITY_BETA: Double = 0.002d //Quadratic viscosity term
  private val v_max: Double = 100.00d //Absolute maximum velocity - big value!
  private var E_STIFF: Double = 10.0d //Elastic stiffness
  private var E_NEAR: Double = 500.0d //Elastic NEAR stiffness
  private val REST_DENSITY: Double = 100.0d //Rest density of the fluid

  var PDIST: Double = pow((VOLUME * 0.5d) / (pi * avKernel), 0.5d).toDouble //Kernel support radius
  private var IP_dist: Double = 3.5d * sqrt(PDIST * PDIST * pi / avKernel) //for 2-D - inter particle distance
  private var MASS: Double = (pi * PDIST * PDIST * REST_DENSITY) / Particle_Count //Mass of particle

  //Kernel smoothing length and it's value squared
  private var h: Double = PDIST * 5.0d
  private var hsq: Double = h * h
  private var hEight: Double = h * h * h * h * h * h * h * h

  //Piston Force variables
  var forceMagnitude, forceDuration, forceDisplacement = 0.0d
  var currentDisplacement = 0.0d
  var movingForward = true
  @volatile var ExternalForceTrigger = false

  //Grid variables
  private var constraint = new Box2D(new Point2D(0.0, 10.0), new Point2D(10.0, 0.0))
  protected val Grid_Divisions: Int = 30 // First Guess
  //Make array to store grid elements like a matrix
  var grid = Array.ofDim[GridElement](n1 = Grid_Divisions, n2 = Grid_Divisions) //[X][Y]

  private var constraintW = .0d
  private var constraintH = .0d
  private var XUnit = .0d
  private var YUnit = .0d

  //Drawing conversion factors...
  var drawScaleX = .0
  var drawScaleY = .0

  //Simulation global variables
  protected var t: Double  = 0.0d
  private var NEIGHBORS = 0
  protected var pList = Array.ofDim[SPHParticle](n1 = Particle_Count)
  var pListBufferConc = new java.util.concurrent.ConcurrentHashMap[Int,(Int,Int)]().asScala

  @volatile var PauseSimulation:Boolean = false

  private var logfile:File = new File("")
  private var logfileX:File = new File("")
  private var logfileY:File = new File("")

  private var islogging: Boolean = false
  private var storeNeighbors: Boolean = false
  private var randomlySample: Boolean = false

  def InitSimulation():Unit = {
    //Create grid for easier assessment of nearest neighbors
    CreateGrid()
    //Initialize SPH particle positions
    initPositions()
    //Add particles to grid - This has already been done but this is a test!
    var p: Int = 0
    while (p < Particle_Count) {
      updateGrid(p)
      pList(p).mass = MASS
      p += 1
    }
  }

  InitSimulation()

  val pointWidth = (PDIST * 0.50d * drawScaleX).toInt
  val pointHeight = (PDIST * 0.50d * drawScaleY).toInt


  def setLogFile(fle: File): Unit = {
    logfile = fle
    logfileX = new File(logfile.getPath() + "_X.csv")
    logfileY = new File(logfile.getPath() + "_Y.csv")
  }

  def logState(state: Boolean) : Unit = {
    islogging = state
  }

  def RandomSample(state: Boolean) : Unit = {
    randomlySample = state
  }

  def StoreNeighbors(state: Boolean) : Unit = {
    storeNeighbors = state
  }

  //===============================
  //Begin methods end constructor
  //===============================

  def updateSimulationParameters(parameters: collection.mutable.Map[String, Double]): Unit ={
    for((label, value) <- parameters){
      label match {
        case "Particles" =>
          Particle_Count = value.toInt
          pList = Array.ofDim[SPHParticle](n1 = Particle_Count)
          pListBufferConc = new java.util.concurrent.ConcurrentHashMap[Int,(Int,Int)]().asScala
        case "Neighbors" =>
          avKernel = value.toInt
          distArr = Array.ofDim[Double](n1 = avKernel) //Vector of particle distances
          indexArr = Array.ofDim[Int](n1 = avKernel) //Vector of particle distances
        case "Duration" => t_f = value
        case "Time Step" => dt = value
        case "Liquid Volume" => VOLUME = value
        case "Container Volume" =>
          PDIST = pow((VOLUME * 0.5d) / (pi * avKernel), 0.5d).toDouble //Kernel support radius
          IP_dist = 3.5d * sqrt(PDIST * PDIST * pi / avKernel) //for 2-D - inter particle distance
          MASS = (pi * PDIST * PDIST * REST_DENSITY) / Particle_Count //Mass of particle
          h = PDIST * 5.0d
          hsq = h * h
          hEight = h * h * h * h * h * h * h * h
          constraint = new Box2D(new Point2D(0.0, 10.0), new Point2D(10.0, 0.0))
        case "Gravity" => gravity = value
        case "Viscosity Sigma" => VISCOSITY_SIGMA = value
        case "Viscosity Beta" => VISCOSITY_BETA = value
        case "Elastic Stiffness" => E_STIFF = value
        case "Elastic Near" => E_NEAR = value
        case _ => println("Unknown parameter!")
      }
    }
    InitSimulation()

  }


  def CreateGrid()= {
    var XBound: Double = 0.0d
    var YBound: Double = 0.0d
    //Set global values
    constraintW = constraint.LowerRight(0) - constraint.UpperLeft(0)
    constraintH = constraint.UpperLeft(1) - constraint.LowerRight(1)

    drawScaleX = (winW.toDouble) / constraintW
    drawScaleY = (winH.toDouble) / constraintH

    XUnit = constraintW/Grid_Divisions
    YUnit = constraintH/Grid_Divisions

    var pnt1: Point2D = new Point2D()
    var pnt2: Point2D = new Point2D()

    //Scroll through each dimension of space.
    var x : Int = 0
    var y : Int = 0
    while(x < Grid_Divisions){
      XBound = x * XUnit + constraint.UpperLeft(1)//Should this be 0 for X?
      y = 0
      while(y < Grid_Divisions){
        YBound = y * YUnit + constraint.LowerRight(1)
        pnt1 = new Point2D(XBound, YBound + YUnit)
        pnt2 = new Point2D(XBound + XUnit, YBound)
        grid(x)(y) = new GridElement( pnt1, pnt2 )
        y += 1
      }//End Y Loop
      x += 1
    }//End X loop
  }//End for method


  def initPositions(): Unit = {
    //User specified a pool of water for initialization
    //This should be the fastest initialization to attain dynamic equilibrium.
    if(InitializationType == "POOL") {
      //Find how many particles can fit across x axis
      val ParticlesInX: Double = constraintW / (IP_dist*1)
      val ParticlesInY:Double = Particle_Count/(1.0*ParticlesInX)

      var xPos:Double = 0.0d;var yPos: Double = 0.0d
      var xBin:Int = 0;var yBin:Int = 0
      var particle: SPHParticle = new SPHParticle()

      var p:Int = 0
      //Iterate through each particle to create its position in 2-space
      while(p < Particle_Count){
        particle = new SPHParticle()
        //First set particle position Math.floor
        xPos = ((p.toDouble) % ParticlesInX) * IP_dist //* 1.5
        yPos = floor(( (p.toDouble) / ParticlesInX ) ) * IP_dist
        particle.pos(0) = xPos
        particle.pos(1) = yPos
        //Now find which portion of the grid the particle belongs too
        xBin  = floor(xPos / XUnit).toInt
        yBin  = floor(yPos / YUnit).toInt
        //Add particle to grid
        grid(xBin)(yBin).addParticleIndex(p)

        //Add Particle to particle list
        pList(p) = particle
        pListBufferConc.put(p, ((particle.pos(0)* drawScaleX).toInt, winH - (particle.pos(1) * drawScaleY).toInt))
        p += 1
      }

    }
    if(InitializationType == "COLUMN"){
      //Find how many particles can fit across x axis
      val ParticlesInX: Double = (constraintW / (IP_dist * 3.0))/1.0
      val ParticlesInY:Double = Particle_Count/(1.0*ParticlesInX)

      var xPos:Double = 0.0d;var yPos: Double = 0.0d
      var xBin:Int = 0;var yBin:Int = 0
      var particle = new SPHParticle()

      var p: Int = 0
      //Iterate through each particle to create its position in 2-space
      while(p < Particle_Count){
        particle = new SPHParticle()
        //First set particle position Math.floor
        xPos = ((p.toDouble) % ParticlesInX) * IP_dist + 3.25
        yPos = floor(( (p.toDouble) / ParticlesInX ) ) * IP_dist + 1.3

        particle.pos(0) = xPos
        particle.pos(1) = yPos
        //Now find which portion of the grid the particle belongs too
        xBin  = floor(xPos / XUnit).toInt
        yBin  = floor(yPos / YUnit).toInt
        //Add particle to grid
        grid(xBin)(yBin).addParticleIndex(p)

        //Add Particle to particle list
        //pList += particle
        pList(p) = particle
        pListBufferConc.put(p, ((particle.pos(0)* drawScaleX).toInt, winH - (particle.pos(1) * drawScaleY).toInt))
        p += 1
      }
    }

  }

  //This function finds a particles current grid position and updates that information to the grid
  // This is done for faster collision and interaction handling.
  def updateGrid(curP: Int){
    val sph:SPHParticle = pList(curP)
    //Find which portion of the grid the particle belongs too
    val xBin: Int = floor(sph.pos(0) / XUnit).toInt
    val yBin: Int = floor(sph.pos(1) / YUnit).toInt
    var x: Int = 0
    var y: Int = 0

    if(xBin >= 0 && xBin < Grid_Divisions &&
      yBin >= 0 && yBin < Grid_Divisions){//With proper collision handling this should not exist.
      //remove particle from previous grid elements guess the plural is possible
      //Scroll through each dimension of space.
      while( x < Grid_Divisions ){
        y = 0
        while( y < Grid_Divisions){
          if(grid(x)(y).IsInside(curP) == true){
            grid(x)(y).removeParticleIndex(curP)
          }
          y += 1
        }//End for grid y
        x += 1
      }//End for grid x

      //Add particle to NEW grid locations
      grid(xBin)(yBin).addParticleIndex(curP)
    }else{
      //WE SHOULD NOT LOSE PARTICLES WITH THE PROPER BOUNDARY ENFORCEMENT!
      println("We lost particle: " + curP)
    }
  }

  //This function scrolls through a list of particles and finds out it's nearest neighboring particles.
  //It sorts the neighbors in order of closeness as well.
  //I guarantee there is a library for this, but it was a good entry level comp. sci. excercise.
  def findNeighbors( pIndex: Int ): Unit = {
    var curParticlePos = pList(pIndex).pos
    //Find grid element that the current particle is in
    var X:Int = -1;    var Y:Int = -1

    var x_it:Int = 0; var y_it:Int = 0

    while( x_it < Grid_Divisions){
      y_it = 0
      while( y_it < Grid_Divisions){
        //Our particle is in this grid element
        if(grid(x_it)(y_it).IsInside(pIndex) == true){
          X = x_it; Y = y_it
          //Break loop...
          x_it = Grid_Divisions; y_it = Grid_Divisions
        }//end while y
        y_it += 1
      }//end while x
      x_it += 1
    }

    //Find neighboring (9 max or 4 min)  grid elements
    //Could change 1 to a bigger 'scope'
    var minX: Int = 0; var maxX: Int = 0
    var minY: Int = 0; var maxY: Int = 0
    //handle edge cases!
    if(X > 0){minX = X - 1;}
    else{minX = X}
    if(X < (Grid_Divisions-1)){maxX = X + 1}
    else{maxX = X}
    if(Y > 0){minY = Y - 1;}
    else{minY = Y}
    if(Y < (Grid_Divisions-1)){maxY = Y + 1}
    else{maxY = Y}

    var sqdistance = 0.0d
    //Now we need to find the nearest neighbor entries.
    //Store our distances and our indices into two list buffers

    var GridX:Int = minX; var GridY:Int = minY
    var storeIndex:Int = 0
    var InsertPopIter: Int = 0
    var move:Int = avKernel - 1
    var biggestDist:Double = -1.0
    var pGrid:Int = 0
    var pnt:Int = 0
    var bucketsize:Int = 0

    //Everything is good up to here
    while(GridX <= maxX){

      GridY = minY
      while(GridY <= maxY){
        if(grid(GridX)(GridY).ParticlesInside.nonEmpty == true){
          pnt = 0
          bucketsize = grid(GridX)(GridY).ParticlesInside.size
          while(pnt < bucketsize) {
            pGrid = grid(GridX)(GridY).ParticlesInside(pnt)
            if (pGrid  != pIndex) {
              sqdistance = curParticlePos.fastSqDistance(pList(pGrid).pos)
              //Is point within kernel distance?
              if(sqdistance.isNaN == false) {//check for nans
                if (sqdistance > 1e-25 && sqdistance < hsq) {
                  if(storeIndex < avKernel){
                    indexArr(storeIndex) = pGrid
                    distArr(storeIndex) = sqdistance
                  }
                  if(storeIndex == avKernel){//We sort once to aid performance...
                    //Sort the indices and the distances by the distance in ascending order...
                    val (distArrT, indexArrT) = distArr.zip(indexArr).sorted.unzip
                    distArr = distArrT
                    indexArr = indexArrT
                    biggestDist = distArr.takeRight(1)(0)
                  }
                  if(storeIndex >= avKernel) {
                    //Remove last elements, and pop in order newest ones
                    //Traverse sorted distances to see if we are smaller(more influential) than the values stored
                    if (biggestDist >= sqdistance ) {//save some time...
                      InsertPopIter = 0
                      while (InsertPopIter < avKernel) {
                        if (distArr(InsertPopIter) > sqdistance) {
                          //copy and pop other elements one index over
                          move = avKernel - 1
                          while (move > InsertPopIter) {
                            distArr(move) = distArr(move - 1)
                            indexArr(move) = indexArr(move - 1)
                            move -= 1
                          }
                          distArr(InsertPopIter) = sqdistance
                          indexArr(InsertPopIter) = pGrid
                          InsertPopIter = avKernel //break loop
                        }
                        InsertPopIter += 1
                      }

                      biggestDist = distArr.takeRight(1)(0)
                    }
                  }

                  storeIndex += 1
                }
              }
            }
            pnt += 1
          }//end while search grid element

        }//end while span grid y
        GridY += 1
      }//end while span grid x
      GridX += 1
    }
  }

  //Update threadsafe display buffer...
  //Obnoxious, but other work arounds are hacky and this is totally readable.
  def updateDisplayBuffer():Unit = {
    var p = 0
    while(p < Particle_Count){
      pListBufferConc.replace(p, ((pList(p).pos(0)* drawScaleX).toInt,
                                winH - (pList(p).pos(1) * drawScaleY).toInt))
      p += 1
    }
  }


  def PauseSimulationLoop(state:Boolean):Unit = {
    PauseSimulation = state
  }

  override def run(): Unit = {
    //throw InterruptedException
    t = 0.0d //reset time to 0
    //The classic N simulation variable. Makes the infinite... approximately finite
    val N_f: Double = (t_f - t) / dt
    //Handle skipping frames
    var skip_count: Int = 0

    //Loop Through each time step until the simulation condition is satisfied.
    var N: Double = 1
    var p: Int = 0
    var q: Boolean = true

    var logStatement:String = ""
    val random = scala.util.Random
    var nextRand = 0.0f
    var randomSamples = new scala.collection.mutable.ArrayBuffer[Int]

    while (N <= N_f && PauseSimulation == false && !Thread.interrupted()) {
      randomSamples = new scala.collection.mutable.ArrayBuffer[Int]
      try {
        //Update Time Step
        t = (t_f - ti) * (N / N_f)

        //Calculate viscosity force
        p = 0 //reset particle iterator
        //Calculate External Forces

        if(ExternalForceTrigger == true){
          PistonForce()
        }

        while (p < Particle_Count) {
          if(islogging == true){
            nextRand = random.nextFloat()
            if(nextRand <= 0.05f || randomlySample == false) {
              randomSamples += p
              logStatement = pList(p).positionString()
            }
          }

          GravityForce(p) //External Forces first
          ViscosityForce(p)

          if(islogging == true){
            if(storeNeighbors == true) {
              for (neighbor <- indexArr) {
                if (nextRand <= 0.05f || randomlySample == false) {
                  if (neighbor >= 0) {
                    logStatement += pList(neighbor).positionString()
                  } else {
                    //If not a neighbor in range, store position as THIS position(Relative position= (0,0))
                    logStatement += pList(p).positionString()
                  }
                }

              }
            }
          }
          if(nextRand <= 0.05f || randomlySample == false) {
            val Xwriter = new FileWriter(logfileX, true)
            Xwriter.write(logStatement)
            Xwriter.write("\n")
            Xwriter.close()
          }
          PseudoLeapFrogStep(p)
          p += 1
        } //End particle loop


        //Update particle positions & grid
        p = 0 //reset particle iterator
        while (p < Particle_Count) {
          PressureForce(p)
          q = true
          while (q == true) {
            q = CheckBounds(p)
          }
          updateGrid(p)
          if(islogging == true){
            if(randomlySample == false || (randomlySample == true && randomSamples.contains(p) == true)) {
              logStatement = pList(p).positionString()
              val Ywriter = new FileWriter(logfileY, true)
              Ywriter.write(logStatement)
              Ywriter.write("\n")
              Ywriter.close()
            }
          }

          p += 1
        } //End particle loop

        //Update particle velocities
        if (N > 1) {
          p = 0 //reset particle iterator
          while (p < Particle_Count) {
            UpdateVelocity(p)
            p += 1
          } //End particle loop
        }

        //redraw the particles after each N frames
        skip_count += 1
        if (skip_count >= skip_frames) {
          updateDisplayBuffer()
          skip_count = 0
        }

        //println(N)
        N += 1.0d
      } catch{//This will likely never work given the heavy lifting of this thread...
          case e: InterruptedException => println("Simulation Thread Interrupted")
      }
    } //End For Time Step
  }

  def PistonForce():Unit = {
    if(currentDisplacement == 0.0d){
      movingForward = true
    }

    if(movingForward == true){
      currentDisplacement += forceDisplacement / forceDuration
      if(currentDisplacement >= forceDisplacement){//relax piston
        movingForward = false
      }
    }else{
      currentDisplacement -= forceDisplacement / forceDuration
      if(currentDisplacement <= 0.0){//stop motion
        ExternalForceTrigger = false
      }
    }
    constraint.UpperLeft.X = currentDisplacement/100.0 * constraint.LowerRight.X
  }


  //Adds effect of gravitational force form acceleration to particles velocity...
  //There are more intricate ways of handling this so I left it as a method even though
  //it is 1 line.
  def GravityForce(curP: Int){
    pList(curP).v(1) +=  gravity * dt
  }

  def blankIndexArray():Unit = {
    var p:Int = 0
    while(p < avKernel){
      indexArr(p) = -1
      p += 1
    }
  }

  //Calculates the pseudo viscosity between two particles
  def ViscosityForce(curP: Int):Unit = {
    var curParticle = pList(curP)
    //Calculate nearest neighbors

    //indexArr = Array.fill[Int](avKernel)(-1)
    blankIndexArray()
    findNeighbors(curP)


    var nb : Int = 0//iterator
    var RelativePosition: Point2D = new Point2D()
    var Vel: Point2D = new Point2D()

    var nbind:Int = 0
    var q:Double = 0.0d
    var V:Double = 0.0d
    var I:Double = 0.0d

    while (nb < avKernel){
      if(indexArr(nb) >=0){
        nbind = indexArr(nb)//saves access 3x
          q = sqrt(distArr(nb)) / h
          if (q.isNaN() == false) {
            RelativePosition.copy(curParticle.pos)
            RelativePosition.sub(pList(nbind).pos)
            RelativePosition.asUnitVector()

            Vel.copy(curParticle.v)
            Vel.sub(pList(nbind).v)
            V = Vel.dot(RelativePosition)

            if (V > 0.0d) {
              I = 0.50d * dt * (1.0d - q) * (VISCOSITY_SIGMA * V + VISCOSITY_BETA * V * V)
              RelativePosition.multiply(I)

              curParticle.v.sub(RelativePosition)
              pList(nbind).v.add(RelativePosition)
            }
          }
      }else{
          nb = avKernel//break loop
      }
      nb += 1
    }//end neighbor influence loop

    pList(curP).v.copy(curParticle.v)
  }

  def PressureForce(curP: Int):Unit = {
    var curParticle = pList(curP)//Save dozens of read accesses

    var runningSum: Double = 0.0d
    var runningSumNear: Double = 0.0d

    //indexArr = Array.fill[Int](avKernel)(-1)
    blankIndexArray()
    findNeighbors(curP)


    var nb : Int = 0//iterator
    var q: Double = 0.0d
    while (nb < avKernel){
      if(indexArr(nb) >=0) {
        q = sqrt(distArr(nb)) / h
        if (q.isNaN() == false) {
          //If everything is OK then update near and far pressures
          val kernel = (1.0d - q) * (1.0d - q)
          runningSum += kernel
          runningSumNear += kernel * (1.0d - q)
        } //End q nan check...
      }
      nb += 1
    }

    //Update pressure and 'near' pressure
    curParticle.P = E_STIFF * ((2.0/pi) *runningSum - REST_DENSITY)
    curParticle.P_near = (2.0/pi) * E_NEAR * runningSumNear

    //No one said SPH was a stable process...
    //Smooth out pressure contributions if they become huge...
    if(abs(curParticle.P + curParticle.P_near) > 1E6 ){
      curParticle.P = curParticle.P/1E6
      curParticle.P_near = curParticle.P_near/1E6
    }

    //Now calculate the influence on the particles velocity...
    nb = 0//iterator
    var RelativePosition: Point2D = new Point2D()
    var clavets:Double = 0.0d
    //var curPos = pList(curP).pos//Save from accessing avKernel times...
    while (nb < avKernel){
      if(indexArr(nb) >=0) {
        q = sqrt(distArr(nb)) / h
        if (q.isNaN() == false) {
          RelativePosition.copy(curParticle.pos)
          RelativePosition.sub(pList(indexArr(nb)).pos)
          RelativePosition.asUnitVector()
          //could optimize this line but this isn't the bottleneck...
          clavets = 0.5d * dt * dt * (curParticle.P * (1.0d - q) + curParticle.P_near * (1.0d - q) * (1.0d - q))
          RelativePosition.multiply(clavets)
          //pList(curP).pos.add(RelativePosition)
          curParticle.pos.add(RelativePosition)
          pList(indexArr(nb)).pos.sub(RelativePosition)

        } //End check for NaN
      }
      nb += 1
    }

    pList(curP) = curParticle


  }

  def PseudoLeapFrogStep(curP: Int):Unit = {
    var curParticle = pList(curP)
    curParticle.posOld.copy(curParticle.pos) // store last position
    //Update position by stored velocity
    curParticle.v.multiply(dt)

    //Constrain velocity in X and Y - This is why I love scala...
    var dim:Int = 0
    while(dim < 2){
      if(curParticle.v(dim) > v_max){
        curParticle.v(dim) = v_max
      }
      if(curParticle.v(dim) < -1.0*v_max){
        curParticle.v(dim) = -v_max
      }
      dim += 1
    }
    //Update position by velocity
    curParticle.pos.add(curParticle.v)

    pList(curP) = curParticle
  }

  def UpdateVelocity(curP: Int):Unit = {
    var difference:Point2D = new Point2D()
    difference.copy(pList(curP).pos)

    difference.sub(pList(curP).posOld)
    //apply pseudo derivative
    difference.divide(dt)
    //Constrain velocity in X and Y - This is why I love scala...
    var dim: Int = 0
    while(dim < 2){
      if(difference(dim) > v_max){
        difference(dim) = v_max
      }
      if(difference(dim) < -1.0*v_max){
        difference(dim) = -v_max
      }
      dim += 1
    }
    //Update position by velocity
    pList(curP).v = difference
  }


  def CheckBounds(curP: Int) : Boolean = {
    var curParticle = pList(curP)
    val damp:Double = 1.0d
    var barrier: Double = 0.0d
    var reflect: Double = 0.0d
    var problem: Boolean = false

    var dim: Int = 0

    var LowConstraint = constraint.UpperLeft(dim)
    var HighConstraint =  constraint.LowerRight(dim)

    while(dim < 2){

      LowConstraint = constraint.UpperLeft(dim)
      HighConstraint =  constraint.LowerRight(dim)

      if(dim == 1){
        LowConstraint = constraint.LowerRight(dim)
        HighConstraint = constraint.UpperLeft(dim)
      }

      if(curParticle.pos(dim) < LowConstraint || curParticle.pos(dim) > HighConstraint) {
        if (dim == 0) {//So Sloppy yuck
          if (curParticle.pos(dim) < LowConstraint) {
            barrier = constraint.UpperLeft(dim)
            reflect = constraint.UpperLeft(dim) + damp * (constraint.UpperLeft(dim) - curParticle.pos(dim))
          } else {
            barrier = constraint.LowerRight(dim)
            reflect = constraint.LowerRight(dim) - damp * (curParticle.pos(dim) - constraint.LowerRight(dim))
          }
        }else{
          if (curParticle.pos(dim) > LowConstraint) {
            barrier = constraint.UpperLeft(dim)
            reflect = constraint.UpperLeft(dim) + damp * (constraint.UpperLeft(dim) - curParticle.pos(dim))
          } else {
            barrier = constraint.LowerRight(dim)
            reflect = constraint.LowerRight(dim) - damp * (curParticle.pos(dim) - constraint.LowerRight(dim))
          }
        }
        problem = true//Particle collided with bound. Flag this event.
        curParticle.posOld(dim) = barrier//Set old position to point of impact
        curParticle.pos(dim) = reflect//Update new position to a reflection
        curParticle.v(dim) *= -1.0 * damp//Change direction of velocity of particle
        if(dim == 0){
          curParticle.posOld(1) = curParticle.pos(1)
        }else{
          curParticle.posOld(0) = curParticle.pos(0)
        }

      }//end if collision
      dim += 1
    }//end for each dimension

    pList(curP) = curParticle

    return problem
  }
}
