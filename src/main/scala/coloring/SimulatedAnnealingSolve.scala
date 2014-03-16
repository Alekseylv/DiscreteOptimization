package coloring

import coloring.ColoringSolver.Solution

/**
 * Created by Aleksey on 16/03/14.
 */
class SimulatedAnnealingSolve(graph: Graph) extends Solve {

  override def solution: Solution = {
    var solve: Greedy = new GreedySolve(graph)
    var solution = solve.solution

    var otherSolve: KempeSearch = new KempeSearch(graph, solution._2, solve.map())
    var otherSolution = otherSolve.solution

    val iterCount = 2000
    var t: Double = 0.5

    var iter = 0
    while (iter < iterCount) {
      if (solution._3 < otherSolution._3 || math.exp((otherSolution._3 - solution._3) / t) > math.random) {
        solve = otherSolve
        solution = otherSolution
      } else {
        otherSolve = new KempeSearch(graph, solution._2, solve.map())
      }

      otherSolution = otherSolve.solution
      iter += 1
      t *= 1.5
    }

    solution
  }
}
