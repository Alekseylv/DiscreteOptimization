package coloring

import scala.util.Random
import scala.collection.{mutable, Set}
import coloring.ColoringSolver.Solution

/**
 * Created by Aleksey on 15/03/14.
 */
class KempeSearch(val graph: Graph, resultInput: Array[Int], colorMap: mutable.HashMap[Int, Int]) extends Solve {

  val result = resultInput.clone()
  var map = colorMap.clone()
  
  def pivot(i: Int, colorToPut: Int, oldColor: Int): Unit = {
    result(i) = colorToPut
    conflictNodes(i).foreach(x => pivot(x, oldColor, colorToPut))
  }

  def conflictNodes(i: Int): Set[Int] = {
    graph.adjacent(i).filter(x => result(x) == result(i))
  }

  override def solution: Solution = {

    //    val color = map.fold((0,0))((a,b) => if (a._2 < b._2) a else b)._1
    //    (0 to graph.V).filter(_ == color).foreach{

    val random = Random.nextInt(result.length)
    val oldColor = result(random)
    val available = map.keySet

    result(random) = available.tail.fold(available.head)((a, b) => if (map(a) > map(b)) a else b)
    val adj = conflictNodes(random)
    if (!adj.isEmpty) {
      adj.foreach(x => pivot(x, oldColor, result(random)))
    }

    map = map.empty
    (0 to graph.V - 1).groupBy(result(_)).mapValues(_.length).foreach(map += _)

    (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)
  }
}
