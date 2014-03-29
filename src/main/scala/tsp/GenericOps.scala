package tsp

import tsp.TspSolver._

/**
 * Created by Aleksey on 29/03/14.
 */
object GenericOps {

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

  def swapOpt(i: Int, j: Int, seq: Array[Int], indexMap: Array[Int]) {
    var start = i + 1
    var end = j - 1
    var temp = 0

    while (end > start) {
      temp = seq(start)
      seq(start) = seq(end)
      seq(end) = temp

      indexMap(seq(start)) = start
      indexMap(seq(end)) = end

      end -= 1
      start += 1
    }
  }

}
