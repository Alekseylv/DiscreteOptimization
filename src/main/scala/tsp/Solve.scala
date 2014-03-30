package tsp

import tsp.TspSolver._

/**
 * Created by Aleksey on 30/03/14.
 */
abstract class Solve(name: String, val N: Int, val data: Data) extends SolutionImprovement {

  val index = getIndex(name)

  def solution: Solution = {
    val sol = improveSolution(solutionSequence)
    (solutionValue(sol), sol)
  }

  def solutionSequence: TraversableOnce[Int]

}
