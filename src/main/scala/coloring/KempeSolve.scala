package coloring

import scala.util.Random

/**
 * Created by Aleksey on 15/03/14.
 */
class KempeSolve(input: Graph, nodeLocalityIndex: TraversableOnce[Int]) extends GreedySolve(input, nodeLocalityIndex) {

  def pivot(i: Int, colorToPut: Int, oldColor: Int): Unit = {
    result(i) = colorToPut
    conflictNodes(i).foreach(x => pivot(x, oldColor, colorToPut))
  }

  def conflictNodes(i: Int): TraversableOnce[Int] = {
    graph.adjacent(i).filter(x => result(x) == result(i))
  }

  override def solution: (Int, Array[Int], Long) = {
    super.solution

    //    val color = map.fold((0,0))((a,b) => if (a._2 < b._2) a else b)._1
    //    (0 to graph.V).filter(_ == color).foreach{

    val random = Random.nextInt(result.length)
    val adj = graph.adjacent(random)
    if (adj.isEmpty)
      assignColor(random)
    else {
      val h = adj.head
      val oldColor = result(random)
      result(random) = result(h)
      result(h) = oldColor
      conflictNodes(random).foreach(x => pivot(x, oldColor, result(random)))
      conflictNodes(h).foreach(x => pivot(x, result(random), oldColor))
    }

    map = map.empty
    (0 to graph.V - 1).groupBy(result(_)).mapValues(_.length).foreach(map += _)


    (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)
  }
}
