package coloring

import coloring.ColoringSolver.Solution
import scala.collection.mutable


/**
 * Created by Aleksey on 12/03/14.
 */
class Solve(val input: Graph) {

  val graph = input
  val result = new Array[Int](graph.V)
  val allColors = (1 to Int.MaxValue).iterator

  val map = new mutable.HashMap[Int, Int]

  private def assignColor(v: Int) {
    val available = mutable.Set.empty ++ map.keySet
    val iter = graph.adjacent(v).iterator

    while (iter.hasNext) {
      available -= result(iter.next())
    }

    if (available.isEmpty) {
      val col = allColors.next()
      result(v) = col
      map += ((col, 1))
    } else {
      val color = available.tail.fold(available.head)((a, b) => if (map(a) > map(b)) a else b)
      result(v) = color
      map += ((color, map(color) + 1))
    }
  }

  def sortedByVertexLocality: TraversableOnce[(Int, Int)] = {
    (0 to graph.V - 1).map(x => (x, graph.adjacent(x).size)).sortBy(- _._2) // hack to sort desc
  }

  def solution: Solution = {

    sortedByVertexLocality.foreach(x => assignColor(x._1))

    (map.keySet.size, result)
  }


}
