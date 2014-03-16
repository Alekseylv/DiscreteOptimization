package coloring

import coloring.ColoringSolver._

/**
 * Created by Aleksey on 15/03/14.
 */
class GreedyIterative(input: Graph, nodeLocalityIndex: TraversableOnce[Int]) extends GreedySolve(input, nodeLocalityIndex) {

  def this(graph: Graph) = {
    this(graph, (0 to graph.V - 1).map(x => (x, graph.adjacent(x).size)).sortBy(-_._2).map(_._1))
  }

  var emptyIterCount: Int = 200

  def decrease = {
    emptyIterCount -= 1
    emptyIterCount > 0
  }

  override def solution: Solution = {
    var solution = super.solution

    var otherSolve: GreedySolve = new GreedySolve(graph, heuristic(vertexLocalityIndex()))
    var otherSolution = otherSolve.solution

    while (solution._3 < otherSolution._3 || decrease) {
      if (solution._3 < otherSolution._3)
        solution = otherSolution
      otherSolve = new GreedySolve(graph, heuristic(otherSolve.vertexLocalityIndex()))

      otherSolution = otherSolve.solution
    }

    solution
  }

}
