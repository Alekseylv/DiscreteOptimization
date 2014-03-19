package coloring

import coloring.ColoringSolver.Solution

/**
 * Created by Aleksey on 16/03/14.
 */
class SimulatedAnnealingSolve(graph: Graph) extends Solve {

  override def solution: Solution = {
    var solve: Greedy = new GreedyIterative(graph)
    var solution = solve.solution

    var otherSolve: LocalSearch = new LocalSearch(graph, solution._2, solve.map())
    var otherSolution = otherSolve.solution

    solution = otherSolution
    val iterCount = 10000
    var t: Double = 0.000001

    var iter = 0
    while (iter < iterCount) {
      if (solution._3 >= otherSolution._3 ||
        math.exp((-(otherSolution._3 - solution._3)) / t) > math.random) {
        solve = otherSolve
        solution = otherSolution
      } else {
        otherSolve = new LocalSearch(graph, solve.result(), solve.map())
      }

      //     println(iter + ": " + (-(otherSolution._3 - solution._3)) + " / " + t + " = " + ((-(otherSolution._3 - solution._3)) / t) + " => " + math.exp((-(otherSolution._3 - solution._3)) / t))

      otherSolution = otherSolve.solution
      iter += 1
      t *= 1.01
    }

    solution
  }
}
