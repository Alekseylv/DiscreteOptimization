package knapsack

/**
 * Created by Aleksey on 07/03/14.
 */

import org.scalacheck._
import Prop.forAll
import knapsack.KnapsackSolver.Data

object ParseSpecification extends Properties("Parsing") with DataGen {

  val precision = 0.001

  property("Items should be sorted by weight (4th field)") = forAll(inputs) {
    data: Data =>
      (data, data.tail).zipped.forall(_._4 <= _._4)
  }

  property("Sorting by first element should give the original order") = forAll(inputs) {
    data: Data =>
      (data.sortBy(_._1), 0 to (data.length + 1)).zipped.forall(_._1 == _)
  }

  property("weighted value must be calculated by dividing value and weight (4th item = 2nd/3rd)") = forAll(inputs) {
    data: Data =>
      data.tail.forall(x => Math.abs(x._4 - (x._2 / x._3.toDouble)) < precision)
  }

}
