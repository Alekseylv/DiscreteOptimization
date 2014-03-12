package coloring

import edu.princeton.cs.algs4.Graph
import coloring.ColoringSolver.Solution
import scala.collection.mutable


/**
 * Created by Aleksey on 12/03/14.
 */
class Solve(val graph: Graph) {

  val colors = mutable.Set[Int]()
  val result = new Array[Int](graph.V)
  val allColors = (1 to Int.MaxValue).iterator

  private def assignColor(v: Int) {
    val adjColor = mutable.Set[Int]()
    val iter = graph.adj(v).iterator()

    while (iter.hasNext) {
      adjColor += result(iter.next())
    }

    val available = colors -- adjColor

    if (available.isEmpty) {
      val col = allColors.next()
      result(v) = col
      colors += col
    } else {
      result(v) = available.head
    }
  }

  def solution: Solution = {

    for (i <- 0 to graph.V() - 1) {
      assignColor(i)
    }

    (colors.size, result)
  }


}
