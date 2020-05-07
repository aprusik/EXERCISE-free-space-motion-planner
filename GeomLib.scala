object GeomLib {
  class point(val x: Double, val y: Double) {
    def -(that: point): vector = new vector(x - that.x, y - that.y)
    def +(that: vector): point = new point(x + that.x, y + that.y)
    def inCircle(c: point, r: Double): Boolean =
      math.sqrt((x - c.x)*(x - c.x) + (y - c.y)*(y - c.y)) < r
  }
  object point {
    def distance(p1: point, p2: point): Double = (p1-p2).mag
  }

  class vector(val x: Double, val y: Double) {
    val mag: Double = math.sqrt(x*x + y*y)
    def normalized: vector = {
      new vector(x/mag, y/mag)
    }
    def *(that: Double): vector = new vector(x*that, y*that)
  }
}
