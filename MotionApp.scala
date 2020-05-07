import stateSearch.RRT

final class MotionApp() {
  val world = new SpaceWorld()

  private val robot = world.makeRobot()

  def printWorld(): Unit = world.print(robot)
}

object MotionApp {
  def main(args: Array[String]): Unit = {
    if (args.length > 0)
      println("This app does not support arguments") // Incorrect usage
    else {
      // read world
      val app = new MotionApp()
//      app.printWorld()

      val alg = new RRT[Robot](app.robot, app.world.getSample,
        GeomLib.point.distance, app.world.checkPath, app.world.toString)

      alg.print(alg.search().get)

//      if (result.isDefined) result.get.printHist()
//      else println("Unable to find solution.")
    }
  }
}