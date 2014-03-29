package tsp

import GenericOps._

/**
 * Created by Aleksey on 22/03/14.
 * Generic trait for solution improvement by removing crossed edges
 */
trait SolutionImprovement extends ClosestNeighbourIndex {

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


  def pred(i: Int) = {
    if (i == 0) N - 1
    else i - 1
  }

  def succ(i: Int) = {
    if (i == N - 1) 0
    else i + 1
  }

  def loopFirst(seq: Array[Int]) {
    var j = 2
    while (j < N - 1) {
      if (intersects(data(seq(N - 1)), data(seq(0)), data(seq(j - 1)), data(seq(j)))) {
        swap(-1, j, seq)
        j = 2
      } else {
        j += 1
      }
    }
  }

  def loopSecond(seq: Array[Int]): Boolean = {
    var i = 0
    var result = false
    while (i < N - 1) {
      var j = i + 3
      while (j < N) {
        if (intersects(data(seq(i)), data(seq(i + 1)), data(seq(j - 1)), data(seq(j)))) {
          swap(i, j, seq)
          result = true
        }
        j += 1

      }
      i += 1
    }

    result
  }

  def improveSolution(sol: TraversableOnce[Int]): TraversableOnce[Int] = {

    val seq = sol.toArray

    loopFirst(seq)
    while (loopSecond(seq)) loopFirst(seq)

    seq
  }
}

//    char get_line_intersection(float p0_x, float p0_y, float p1_x, float p1_y,
//      float p2_x, float p2_y, float p3_x, float p3_y, float *i_x, float *i_y)
//    {
//      float s1_x, s1_y, s2_x, s2_y;
//      s1_x = p1_x - p0_x;     s1_y = p1_y - p0_y;
//      s2_x = p3_x - p2_x;     s2_y = p3_y - p2_y;
//
//      float s, t;
//      s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y);
//      t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y);
//
//      if (s >= 0 && s <= 1 && t >= 0 && t <= 1)
//      {
//        // Collision detected
//        if (i_x != NULL)
//          *i_x = p0_x + (t * s1_x);
//        if (i_y != NULL)
//          *i_y = p0_y + (t * s1_y);
//        return 1;
//      }
//
//      return 0; // No collision
//    }