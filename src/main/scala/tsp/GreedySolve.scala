package tsp

import tsp.TspSolver._
import scala.collection.mutable

/**
 * Created by Aleksey on 21/03/14.
 */
class GreedySolve(val N: Int, val data: Data) {

  def solutionValue(solution: TraversableOnce[Int]): Double = {
    var result: Double = 0
    val iter = solution.toIterator

    val first = iter.next()
    var current = iter.next()

    result += length(first, current)

    while (iter.hasNext) {
      val temp = iter.next()
      result += length(current, temp)
      current = temp
    }

    result += length(first, current)
    result
  }

  def length(i: Int, j: Int): Double = {
    math.sqrt(sqr(data(i)._1 - data(j)._1) + sqr(data(i)._2 - data(j)._2))
  }

  def sqr(i: Double) = i * i

  def solution: Solution = {
    val set = new mutable.HashSet[Int]() ++ (1 to N - 1)
    val sol = new mutable.MutableList[Int]
    sol += 0
    var head = 0

    while (!set.isEmpty) {
      val smallest = set.tail.fold(set.head)((a, b) => if (length(head, a) < length(head, b)) a else b)
      set -= smallest
      sol += smallest
      head = smallest
    }


    (solutionValue(sol), sol)
  }
}
