package tsp

import tsp.TspSolver._
import scala.collection.mutable

/**
 * Created by Aleksey on 21/03/14.
 */
class GreedySolve(val N: Int, val data: Data) extends SolutionImprovement {

  val currentSolution = new mutable.MutableList[Int]()

  def solution: Solution = {
    val set = new mutable.HashSet[Int]() ++ (1 to N - 1)
    currentSolution += 0
    var head = 0

    while (!set.isEmpty) {
      val smallest = set.tail.fold(set.head)((a, b) => if (length(head, a) < length(head, b)) a else b)
      set -= smallest
      currentSolution += smallest
      head = smallest
    }


    improveSolution(currentSolution)
  }
}
