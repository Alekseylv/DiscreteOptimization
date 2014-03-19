package coloring

import scala.collection.mutable
import coloring.ColoringSolver.Solution
import scala.util.Random

/**
 * Created by Aleksey on 16/03/14.
 */
class LocalSearch(val graph: Graph, resultInput: Array[Int], colorMap: mutable.HashMap[Int, Int]) extends Greedy {

  val result = resultInput.clone()
  var map = colorMap.clone()

  override def solution: Solution = {
    val vertex = graph.randomVertex()
    val color = result(vertex)
    val oldValue = map(color)
    if (oldValue == 1) {
      map.-=(color)
    } else {
      map.+=((color, oldValue - 1))
    }

    val newColor = map.keySet.toArray.apply(Random.nextInt(map.keySet.size))
    val colorVal = map(newColor)
    map.+=((newColor, colorVal + 1))
    result(vertex) = newColor


    //    map = map.empty
    //    (0 to graph.V - 1).groupBy(result(_)).mapValues(_.length).foreach(map += _)

    val badEdgeValue = (0 to result.length - 1).foldRight(0)((a, b) => 2 * conflictNodes(a).size * map(result(a)) + b)
    (map.keySet.size, result, badEdgeValue - map.values.fold(0)((a, b) => b * b + a).toLong)
  }
}


