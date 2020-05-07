import GeomLib.point

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

class SpaceWorld() {
  private final val STEP_DISTANCE: Double = 0.005
  val cols: Int = StdIn.readInt()
  val rows: Int = StdIn.readInt()
  val maxRows: Int = rows - 1
  val world: Array[Array[Boolean]] = makeWorld() // world array
  val startX: Double = StdIn.readDouble()
  val startY: Double = StdIn.readDouble()
  val goalX: Double = StdIn.readDouble()
  val goalY: Double = StdIn.readDouble()
  var walls: mutable.Set[(Int, Int)] = _
  // make robot at start position
  def makeRobot(): Robot = Robot(startX, startY)

  // populate world
  private def makeWorld(): Array[Array[Boolean]] = {
    val newWorld = Array.ofDim[Boolean](cols, rows)
    // populate world array
    walls = mutable.Set.empty
    for (y <- maxRows to 0 by -1) {
      var x = 0
      for (col <- StdIn.readLine()) { // read next row, divide into columns
        newWorld(x)(y) = if (col == '#') {walls += ((x,y)); true} else false //add entry into world
        x += 1
      }
    }
    newWorld
  }

  // print initial world and size
  def print(robot: Robot): Unit = {
    // Print world size
    println("size: " + rows + "x" + cols)

    // print world
    for (y <- maxRows to 0 by -1) {
      var row = ""
      for (x <- 0 until cols)
        if (goalX.toInt == x && goalY.toInt == y) row += "G "
        else if (robot.x.toInt == x && robot.y.toInt == y) row += "@ " // check for robot
        else row += {if (world(x)(y)) "# " else "_ "}
      println(row)
    }
  }

  // print robot coords
  def toString(robot: Robot): String = robot.x + " " + robot.y

  // returns true if this cell is blocked
  def apply(x: Int, y: Int): Boolean = world(x)(y)

  // returns some true if goal state, false if path, empty if blocked
  def checkPath(r1: Robot, r2: Robot): Option[(Boolean, Option[Robot])] = {
    val r = new point(r1.x, r1.y)
    val p = new point(r2.x, r2.y)
    val v = p - r
    val u = v.normalized

    var d = 0.0
    @tailrec
    def step(nextP: point): Option[(Boolean, Option[Robot])] = {
//      println(nextP.x + " " + nextP.y) // debugging
      d += STEP_DISTANCE
      if (d > v.mag) Some((false, None))
      else if (walls.exists((wall: (Int, Int)) =>
        wall._1 == nextP.x.toInt && wall._2 == nextP.y.toInt)) None // checks if point in wall
      else if (nextP.inCircle(new point(goalX, goalY), 0.1))
        Some((true, Some(Robot(nextP.x, nextP.y))))
      else step(r + u * d)
      }
    step(r)
    }

  def getSample(unit: Unit): Robot = {
    val rand = scala.util.Random
    if (rand.nextInt(100) <= 5) Robot(goalX, goalY)
    else Robot(rand.nextInt(cols) + rand.nextDouble,
      rand.nextInt(rows) + rand.nextDouble)
  }
}

case class Robot(override val x: Double, override val y: Double)
  extends GeomLib.point(x: Double, y: Double)
