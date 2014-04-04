package tsp

import tsp.TspSolver._
import GenericOps._
import scala.annotation.tailrec


/**
 * Created by Aleksey on 23/03/14.
 */
class TwoOpt(name: String, N: Int, data: Data) extends GreedySolve(name, N, data) {

  var indexMap = new Array[Int](N)

  var seq: Array[Int] = null

  override def solutionSequence: TraversableOnce[Int] = {
    seq = super.solutionSequence.toArray

    var iter = 0

    while (iter < 200) {
      var i = 0

      while (i < N) {
        indexMap(seq(i)) = i
        i += 1
      }

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

  }

  def findMin(initial: Int, used: Int*): Int = {
    var other = initial
    while (used.contains(other)) other += 1
    other
  }

  def bestPermutation(a1: Int, a2: Int, b1: Int, b2: Int, c1: Int, c2: Int, d1: Int, d2: Int): (List[(Int, Int)], Double) = {

    var best = List[(Int, Int)]()
    var bestLength = Double.MaxValue

    val arr = Array(a1, a2, b1, b2, c1, c2, d1, d2)
    val map = Array(a2, a1, b2, b1, c2, c1, d2, d1)

    var i = 2
    while (i < arr.length) {

      var j = 2
      while (j < arr.length) {
        if (i != j) {

          val k1 = findMin(2, i, j)
          var k = k1 + 1
          while (k < arr.length) {
            if (k != i && k != j && arr(k) != map(k1)) {

              val p1 = findMin(3, i, j, k1, k)
              var p = p1 + 1
              while (p < arr.length) {
                if (p != i && p != j && p != k && p != k1 && arr(p) != map(p1)) {

                  val len = length(arr(0), arr(i)) + length(arr(1), arr(j)) + length(arr(k1), arr(k)) + length(arr(p1), arr(p))
                  if (len < bestLength) {
                    bestLength = len
                    best = List((arr(0), arr(i)), (arr(1), arr(j)), (arr(k1), arr(k)), (arr(p1), arr(p)))
                  }
                }

                p += 1
              }
            }

            k += 1
          }
        }

        j += 1
      }

      i += 1
    }

    (best, bestLength)
  }
}