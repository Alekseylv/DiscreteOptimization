package tsp

import tsp.TspSolver._
import GenericOps._


/**
 * Created by Aleksey on 23/03/14.
 */
class TwoOpt(name: String, N: Int, data: Data) extends GreedySolve(name, N, data) {

  override def solutionSequence: TraversableOnce[Int] = {
    val seq = super.solutionSequence.toArray

    var i = 0
    println("Start 2-opt")

    while (i < N) {

      val changeFromLength = length(seq(i), seq(succ(i))) //+ length(seq(pred(j)), seq(j))

      //TODO this is all wrong
      val item = (index(seq(i)) -- List(seq(succ(i)), seq(pred(i)))).foldLeft((-1, Double.MaxValue)) {
        (old, j) =>
          val changeToLength = length(seq(i), seq(pred(j))) + length(seq(succ(i)), seq(j))

          if (old._2 > changeToLength && changeFromLength + length(seq(j), seq(pred(j))) > changeToLength) {
            (j, changeToLength)
          } else {
            old
          }
      }

      if (item._1 != -1) {
        swap(i, item._1, seq)
      }

      i += 1
    }
    println("End 2-opt")
    seq
  }
}

//val seq = super.solutionSequence.toArray
//
//var j = 2
//var bestSwapIndex = -1
//var minSwapLength = Double.MaxValue
//var changeFromLength = 0.0
//
//val start = 0
//var i = start
//while (succ(i) != start) {
//j = succ(succ(succ(i)))
//if (succ(j) != start)
//changeFromLength = length(seq(i), seq(succ(i))) + length(seq(pred(j)), seq(j))
//bestSwapIndex = -1
//minSwapLength = Double.MaxValue
//while (succ(j) != start) {
//val changeToLength = length(seq(i), seq(pred(j))) + length(seq(succ(i)), seq(j))
//if (changeFromLength > changeToLength && minSwapLength > changeToLength) {
//bestSwapIndex = j
//minSwapLength = changeToLength
//}
//j =  succ(j)
//}
//if (bestSwapIndex != -1) {
//swap(i, bestSwapIndex, seq)
//}
//i = succ(i)
//}
//
//seq
