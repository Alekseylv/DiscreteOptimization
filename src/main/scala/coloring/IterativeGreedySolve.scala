package coloring

import coloring.ColoringSolver.Solution
import scala.collection.immutable.IndexedSeq
import scala.util.Random


/**
 * Created by Aleksey on 12/03/14.
 */
class IterativeGreedySolve(input: Graph, nodeLocalityIndex: TraversableOnce[Int]) extends GreedySolve(input, nodeLocalityIndex) {

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

  override def solution: Solution = {
    nodeLocalityIndex.foreach(x => assignColor(x))

    var solution = (map.keySet.size, result, map.values.fold(0)((a, b) => b * b + a).toLong)

    var otherSolve: GreedySolve = new GreedySolve(graph, reversed(vertexLocalityIndex(solution)))
    var otherSolution = otherSolve.solution

    while (solution._3 < otherSolution._3 || decrease) {
      if (solution._3 < otherSolution._3) {
        otherSolve = new KempeSolve(graph, heuristic(otherSolve.vertexLocalityIndex(solution)))
        solution = otherSolution
      } else {
        otherSolve = new GreedySolve(graph, heuristic(otherSolve.vertexLocalityIndex(solution)))
      }
      otherSolution = otherSolve.solution
    }

    solution
  }


}
