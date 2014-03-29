package tsp

import tsp.TspSolver._
import GenericOps._


/**
 * Created by Aleksey on 23/03/14.
 */
class TwoOpt(name: String, N: Int, data: Data) extends GreedySolve(name, N, data) {

  override def solutionSequence: TraversableOnce[Int] = {
    val seq = super.solutionSequence.toArray

    val indexMap: Array[Int] = new Array[Int](N)
    var i = 0

    while (i < N) {
      indexMap(seq(i)) = i
      i += 1
    }

    //    println("Start 2-opt")

    i = 0
    while (i < N) {

      val changeFromLength = length(seq(i), seq(succ(i))) //+ length(seq(pred(j)), seq(j))

      val item = (index(seq(i)) -- List(seq(succ(i)), seq(pred(i)))).foldLeft((-1, Double.MaxValue)) {
        (old, j) =>
          val changeToLength = length(seq(i), seq(pred(indexMap(j)))) + length(seq(succ(i)), j)

          if (old._2 > changeToLength && changeFromLength + length(j, seq(pred(indexMap(j)))) > changeToLength) {
            (indexMap(j), changeToLength)
          } else {
            old
          }
      }

      if (item._1 != -1) {
        swapOpt(i, item._1, seq, indexMap)
      }

      i += 1
    }
    //    println("End 2-opt")
    seq
  }
}