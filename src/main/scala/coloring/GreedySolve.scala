package coloring

/**
 * Created by Aleksey on 15/03/14.
 */

import scala.collection.mutable
import coloring.ColoringSolver.Solution
import scala.util.Random


class GreedySolve(val input: Graph, val nodeLocalityIndex: TraversableOnce[Int]) {

  import scala.collection.immutable.IndexedSeq

  val graph = input
  val result = new Array[Int](graph.V)
  val allColors = (1 to Int.MaxValue).iterator

  var map = new mutable.HashMap[Int, Int]

  protected def assignColor(v: Int) {
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

  def vertexLocalityIndex(solution: Solution): Map[Int, IndexedSeq[Int]] = {
    (0 to result.length - 1).groupBy(x => result(x))
  }

  def largestFirst(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.toList.sortBy(-_.length).flatten
  }

  def random(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    Random.shuffle(index.values).map(Random.shuffle(_)).flatten.toSeq
  }

  def reversed(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.map(_.reverse).flatten.toSeq
  }

  def solution: Solution = {
    nodeLocalityIndex.foreach(x => assignColor(x))

    (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)
  }
}

