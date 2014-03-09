/**
 * Created by Aleksey on 08/03/14.
 */

import knapsack.DataGen
import knapsack.Solver
import Solver.Data
import org.scalacheck._
import Prop.forAll
import knapsack.Solver

object HelperFunctionSpecification extends Properties("Helper function Test") with DataGen {

  val capacity = 10000

  property("weight should not exceed capacity in estimate") = forAll(inputs) {
    d: Data =>
      Solver.estimate(d.length, d, capacity)._2 <= capacity
  }

  property("estimate should take only n first elements") = forAll(inputs) {
    d: Data =>
      val randInt = Gen.choose(0, d.length).sample.get
      Solver.estimate(d.length - randInt, d, capacity) == Solver.estimate(d.length - randInt, d.take(d.length - randInt), capacity)
  }

  property("reversely sorted data should have a smaller or equal to estimate value") = forAll(inputs) {
    d: Data =>
      Solver.estimate(d.length, d, capacity)._1 >= Solver.estimate(d.length, d.reverse, capacity)._1
  }

  // Compression algorithm: Run length encoding (there are long sequences of 0's in typical input data)
  property("Compressing and decompressing must yield the same value") = forAll(numberSequences) {
    l: List[Int] =>
      val compressed = l.foldLeft(List[Int]())((a, b) => Solver.addDigit(b, a))
      l == Solver.decompress(compressed)
  }

}

