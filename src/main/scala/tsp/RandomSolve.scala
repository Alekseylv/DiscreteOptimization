package tsp

import tsp.TspSolver._
import scala.util.Random

/**
 * Created by Aleksey on 30/03/14.
 */
class RandomSolve(name: String, N: Int, data: Data) extends Solve(name, N, data) {

  def solutionSequence: TraversableOnce[Int] = Random.shuffle((0 to N - 1).toList)


}
