package tsp

import org.scalacheck._
import Prop.forAll
import scala.util.Random

/**
 * Created by Aleksey on 22/03/14.
 */
object SolverSpec extends Properties("Properties for methods in solvers") with DataGen {

  property("swapping entire array is equal to reversing it") = forAll {
    x: Array[Int] =>
      val old = x.clone()
      new GreedySolve(x.length, null).swap(-1, x.length, x)
      x.toList == old.reverse.toList
  }

  property("swapping subsequence") = forAll {
    x: Array[Int] =>
      val old = x.clone()
      val n = Random.nextInt(x.length + 1)
      new GreedySolve(x.length, null).swap(-1, n, x)
      x.toList.take(n) == old.toList.take(n).reverse
  }

  property("Intersected lines should intersect") = forAll(intersectedLines) {
    x => new GreedySolve(0, null).intersects(x._1, x._2, x._3, x._4)
  }

}
