package tsp

import tsp.TspSolver._
import scala.util.Random
import tsp.GenericOps._

/**
 * Created by Aleksey on 30/03/14.
 */
class SimulatedAnnealing(name: String, N: Int, data: Data) extends RandomSolve(name, N, data) {

  override def solutionSequence: TraversableOnce[Int] = {
    val seq = super.solutionSequence.toArray
    var value = solutionValue(seq)
    val indexMap: Array[Int] = new Array[Int](N)

    var k = 0

    while (k < N) {
      indexMap(seq(k)) = k
      k += 1
    }

    val iterCount = 100000
    var t: Double = 0.001

    var iter = -1
    do {
      val i = Random.nextInt(N)

      val items = index(seq(i)) -- List(seq(succ(i)), seq(pred(i)))
      val j = indexMap(items.toArray.apply(Random.nextInt(items.size)))


      swapOpt(i, j, seq, indexMap)

      //      val otherValue = value - length(seq(i), seq(succ(i))) - length(j, seq(pred(indexMap(j)))) +
      //        length(seq(i), seq(pred(indexMap(j)))) + length(seq(succ(i)), j)

      val otherValue = solutionValue(seq)

      if (value > otherValue) {
        value = otherValue
        //     swapOpt(i, j, seq, indexMap)
        println(iter + ": improve")
        assert(math.abs(solutionValue(seq) - value) < 0.0001)
      } else {
        val rand = math.random
        println(iter + ": " + (otherValue - value) + " / " + t + " = " + ((otherValue - value) / t) + " == " + math.exp((otherValue - value) / t) + " => " + (math.exp((otherValue - value) / t) - 1 > rand))

        if (math.exp((otherValue - value) / t) - 1 > rand) {
          value = otherValue
          //        swapOpt(i, j, seq, indexMap)
        } else {
          swapOpt(i, j, seq, indexMap)
          assert(math.abs(solutionValue(seq) - value) < 0.0001)
        }
      }
      iter += 1
      t *= 1.001
      t += 0.00001
    } while (iter < iterCount)

    seq
  }
}