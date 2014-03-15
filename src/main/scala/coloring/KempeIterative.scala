package coloring

import coloring.ColoringSolver._

/**
 * Created by Aleksey on 15/03/14.
 */
class KempeIterative(input: Graph, nodeLocalityIndex: TraversableOnce[Int]) extends KempeSolve(input, nodeLocalityIndex) {

  var emptyIterCount: Int = 20000

  def decrease = {
    emptyIterCount -= 1
    emptyIterCount > 0
  }

  override def solution: Solution = {
    var solution = super.solution

    var otherSolve: GreedySolve = new KempeSolve(graph, heuristic(vertexLocalityIndex(solution)))
    var otherSolution = otherSolve.solution

    while (solution._3 < otherSolution._3 || decrease) {
      if (solution._3 < otherSolution._3)
        solution = otherSolution
      otherSolve = new KempeSolve(graph, heuristic(otherSolve.vertexLocalityIndex(solution)))

      otherSolution = otherSolve.solution
    }

    solution
  }

}
