package coloring

import coloring.ColoringSolver.Solution
import scala.collection.mutable
import scala.collection.immutable.IndexedSeq
import scala.util.Random


/**
 * Created by Aleksey on 12/03/14.
 */
class Solve(val input: Graph, val nodeLocalityIndex: TraversableOnce[Int]) {

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

  private def vertexLocalityIndex(solution: Solution): Map[Int, IndexedSeq[Int]] = {
    (0 to result.length - 1).groupBy(x => result(x))
  }

  private def largestFirst(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.toList.sortBy(-_.length).flatten
  }

  private def random(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    Random.shuffle(index.values).map(Random.shuffle(_)).flatten.toSeq
  }

  private def reversed(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.map(_.reverse).flatten.toSeq
  }

  private def internalSolve: Solution = {
    nodeLocalityIndex.foreach(x => assignColor(x))

    (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)
  }

  private def heuristic(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    val probability = Random.nextInt(130)

    if (probability < 30) random(index)
    else if (probability < 80) reversed(index)
    else largestFirst(index)
  }

  var emptyIterCount: Int = 20

  def decrease = {
    emptyIterCount -= 1
    emptyIterCount > 0
  }

  def solution: Solution = {
    nodeLocalityIndex.foreach(x => assignColor(x))

    var solution = (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)

    var otherSolve = new Solve(graph, reversed(vertexLocalityIndex(solution)))
    var otherSolution = otherSolve.internalSolve

    while (solution._3 < otherSolution._3 || decrease) {
      solution = otherSolution
      otherSolve = new Solve(graph, heuristic(otherSolve.vertexLocalityIndex(solution)))
      otherSolution = otherSolve.internalSolve
    }

    solution
  }


}
