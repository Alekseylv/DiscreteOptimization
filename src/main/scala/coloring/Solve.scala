package coloring

import coloring.ColoringSolver.Solution
import scala.collection.mutable


/**
 * Created by Aleksey on 12/03/14.
 */
class Solve(val input: Graph) {

  val graph = input
  val colors = mutable.Set[Int]()
  val result = new Array[Int](graph.V)
  val allColors = (1 to Int.MaxValue).iterator

  private def assignColor(v: Int) {
    val available = colors.clone()
    val iter = graph.adjacent(v).iterator

    while (iter.hasNext) {
      available -= result(iter.next())
    }

    if (available.isEmpty) {
      val col = allColors.next()
      result(v) = col
      colors += col
    } else {
      result(v) = available.head
    }
  }

  def sortedByVertexLocality:TraversableOnce[(Int,Int)] = {
    (0 to graph.V - 1).map(x => (x, graph.adjacent(x).size)).sortBy(- _._2) // hack to sort desc
  }

  def solution: Solution = {
    sortedByVertexLocality.foreach(x => assignColor(x._1))

    (colors.size, result)
  }


}
