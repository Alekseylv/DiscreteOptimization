package coloring

/**
 * Created by Aleksey on 15/03/14.
 */

import scala.collection.mutable
import coloring.ColoringSolver.Solution
import scala.collection.Set


class GreedySolve(val input: Graph, val nodeLocalityIndex: TraversableOnce[Int], val result: Array[Int]) extends Greedy {

  def this(input: Graph, nodeIndex: TraversableOnce[Int]) = this(input, nodeIndex, new Array[Int](input.V))

  def this(graph: Graph) = {
    this(graph, (0 to graph.V - 1).map(x => (x, graph.adjacent(x).size)).sortBy(-_._2).map(_._1), new Array[Int](graph.V))
  }

  val graph = input
  val allColors = (1 to Int.MaxValue).iterator

  var map = new mutable.HashMap[Int, Int]

  def availableColorsTo(v: Int): Set[Int] = {
    val available = mutable.Set.empty ++ map.keySet
    val iter = graph.adjacent(v).iterator

    while (iter.hasNext) {
      available -= result(iter.next())
    }

    available
  }

  def chooseColor(available: Set[Int]) = available.tail.fold(available.head)((a, b) => if (map(a) > map(b)) a else b)

  protected def assignColor(v: Int) {
    val available = availableColorsTo(v)

    if (available.isEmpty) {
      val col = allColors.next()
      result(v) = col
      map += ((col, 1))
    } else {
      val color = chooseColor(available)
      result(v) = color
      map += ((color, map(color) + 1))
    }
  }



  override def solution: Solution = {
    nodeLocalityIndex.foreach(x => assignColor(x))

    (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)
  }
}

