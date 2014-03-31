package tsp

import tsp.TspSolver._
import GenericOps._
import scala.annotation.tailrec


/**
 * Created by Aleksey on 23/03/14.
 */
class TwoOpt(name: String, N: Int, data: Data) extends GreedySolve(name, N, data) {

  override def solutionSequence: TraversableOnce[Int] = {
    val seq = super.solutionSequence.toArray

    val indexMap: Array[Int] = new Array[Int](N)
    var iter = 0

    while (iter < 200) {
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

      iter += 1
    }
    seq
  }

  /**
   * Bear with me on this...
   * @param seq solution sequence
   * @param indexMap reverse map from node to index in solution
   */
  def fourOpt(seq: Array[Int], indexMap: Array[Int]) {

    println("starting 4 op")

    var i = 0

    while (i < N) {
      val a = seq(i)
      val a2 = seq(succ(i))
      val iter = index(a).iterator

      @tailrec
      def secondIteration() {
        val b = iter.next()

        if (a2 == b) return
        if (a < b) {
          val b2 = seq(succ(indexMap(b)))
          val iter2 = index(b).iterator

          @tailrec
          def thirdIteration() {
            val c = iter2.next()

            if (b2 == c) return
            if (b < c && c != a) {
              val c2 = seq(succ(indexMap(c)))
              val iter3 = index(c).iterator

              @tailrec
              def fourthIteration() {
                val d = iter3.next()

                if (c2 == d) return
                if (c < d && d != a && d != b) {
                  val d2 = seq(succ(indexMap(d)))
                  //TODO call best permutation
                  //TODO commit if better
                }


                if (iter3.hasNext) fourthIteration()
              }

              fourthIteration()
            }

            if (iter2.hasNext) thirdIteration()
          }

          thirdIteration()
        }

        if (iter.hasNext) secondIteration()
      }

      secondIteration()

      i += 1
    }

    println("Done 4 opt")
  }
}