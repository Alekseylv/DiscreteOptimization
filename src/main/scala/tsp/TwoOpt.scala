package tsp

import tsp.TspSolver._

/**
 * Created by Aleksey on 23/03/14.
 */
class TwoOpt(N: Int, data: Data) extends GreedySolve(N, data) {


  override def solution: (Double, TraversableOnce[Int]) = {
    improveSolution(solutionSequence)
  }

  override def solutionSequence: TraversableOnce[Int] = {
    val seq = super.solutionSequence.toArray

    var j = 2
    var bestSwapIndex = -1
    var minSwapLength = Double.MaxValue
    var changeFromLength = length(seq(N - 1), seq(0)) + length(seq(j - 1), seq(j))
    while (j < N - 1) {
      val changeToLength = length(seq(N - 1), seq(j - 1)) + length(seq(0), seq(j))
      if (changeFromLength > changeToLength && minSwapLength > changeToLength) {
        bestSwapIndex = j
        minSwapLength = changeToLength
      }
      j += 1
    }

    if (bestSwapIndex != -1) {
      swap(-1, bestSwapIndex, seq)
    }

    var i = 0
    while (i < N - 1) {
      j = i + 3
      if (j < N)
        changeFromLength = length(seq(i), seq(i + 1)) + length(seq(j - 1), seq(j))
      bestSwapIndex = -1
      minSwapLength = Double.MaxValue
      while (j < N) {
        val changeToLength = length(seq(i), seq(j - 1)) + length(seq(i + 1), seq(j))
        if (changeFromLength > changeToLength && minSwapLength > changeToLength) {
          bestSwapIndex = j
          minSwapLength = changeToLength
        }
        j += 1
      }
      if (bestSwapIndex != -1) {
        swap(i, bestSwapIndex, seq)
      }
      i += 1
    }

    seq
  }
}
