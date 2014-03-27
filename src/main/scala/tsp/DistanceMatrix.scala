package tsp

import tsp.TspSolver.Data

/**
 * Created by Aleksey on 27/03/14.
 */
class DistanceMatrix(val N: Int, data: Data) {

  if (N <= 0) throw new java.lang.IllegalArgumentException("N can not be less than 0")

  private[this] val vector = new Array[Double](N * (N + 1) / 2)

  {
    var i = 0
    var j = 0
    var k = 0
    while (i < N) {
      j = 0
      while (j <= i) {
        vector(k) = math.sqrt(sqr(data(i)._1 - data(j)._1) + sqr(data(i)._2 - data(j)._2))
        k += 1
        j += 1
      }
      i += 1
    }
  }

  def apply(i: Int, j: Int): Double = {
    if (j > i) apply(j, i)
    else vector(i * (i + 1) / 2 + j)
  }

  def sqr(i: Double) = i * i
}
