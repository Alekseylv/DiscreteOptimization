package tsp

import tsp.TspSolver.Node

/**
 * Created by Aleksey on 22/03/14.
 * Generic trait for solution improvement by removing crossed edges
 */
trait SolutionImprovement extends ClosestNeighbourIndex {

  def currentSolution: TraversableOnce[Int]

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

  /**
   * Basic linear math
   * @param p1 start point of line 1
   * @param p2 end point of line 1
   * @param p3 start point of line 2
   * @param p4 end point of line 2
   * @return do the lines intersect
   */
  def intersects(p1: Node, p2: Node, p3: Node, p4: Node): Boolean = {

    val firstLineX = p2._1 - p1._1
    val firstLineY = p2._2 - p1._2

    val secondLineX = p4._1 - p3._1
    val secondLineY = p4._2 - p3._2

    val s = (-firstLineY * (p1._1 - p3._1) + firstLineX * (p1._2 - p3._2)) / (-secondLineX * firstLineY + firstLineX * secondLineY)
    val t = (secondLineX * (p1._2 - p3._2) - secondLineY * (p1._1 - p3._1)) / (-secondLineX * firstLineY + firstLineX * secondLineY)

    s >= 0 && s <= 1 && t >= 0 && t <= 1
  }

  def swap(i: Int, j: Int, seq: Array[Int]) {
    var start = i + 1
    var end = j - 1
    var temp = 0

    while (end > start) {
      temp = seq(start)
      seq(start) = seq(end)
      seq(end) = temp
      end -= 1
      start += 1
    }
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
    while (i < N - 1) {
      var j = i + 3
      while (j < N) {
        if (intersects(data(seq(i)), data(seq(i + 1)), data(seq(j - 1)), data(seq(j)))) {
          swap(i, j, seq)
          return true
        } else {
          j += 1
        }
      }
      i += 1
    }
    false
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