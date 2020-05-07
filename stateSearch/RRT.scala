package stateSearch

import scala.annotation.tailrec

class RRT[A](start: A, getSample: Unit => A, dist: (A, A) => Double,
             check: (A, A) => Option[(Boolean, Option[A])], string: A => String)
  extends StateSearch[A] {

  var nodes: List[RRTNode] = List(RRTNode(start, 1, None))
  var edges: List[(A, A)] = List.empty

  @tailrec
  final override def search(): Option[RRTNode] = {
    val sample = getSample()

    val near = nodes.min(Ordering.by((n: RRTNode) => dist(n.data, sample)))

    lazy val sampleNode = RRTNode(sample, near.depth + 1, Some(near))

    check(near.data, sample) match {
      case None => search()                             // Collision detected
      case Some((true, goal)) =>
        edges = (goal.get, near.data) +: edges
        Option(RRTNode(goal.get, near.depth+1, Some(near))) // Goal Found
      case Some((false, None)) =>                       // No Collision, No Goal
        nodes = sampleNode +: nodes
        edges = (sample, near.data) +: edges
        search()
      case _ => throw new IllegalStateException("illegal tuple")
    }
  }

  def print(node: RRTNode): Unit = {
    println(node.depth)
    def printPath(node: RRTNode): Unit = {
      if (node.parent.isDefined) printPath(node.parent.get)
      println(string(node.data))
    }
    printPath(node)

    println(edges.size)
    for (edge <- edges) println(string(edge._1) + " " + string(edge._2))
  }

  protected case class RRTNode(data: A, depth: Int, parent: Option[RRTNode]) extends Node
}
