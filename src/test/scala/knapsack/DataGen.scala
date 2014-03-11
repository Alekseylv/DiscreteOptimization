package knapsack

/**
 * Created by Aleksey on 08/03/14.
 */

import org.scalacheck._
import knapsack.Solver._

trait DataGen {

  val arb: Gen[Int] = Gen.choose(1, 1000)

  def getLines(n: Int): List[String] = {
    for (i <- 0 to n) yield arb.sample.get + " " + arb.sample.get
  }.toList

  def numberToList(times: Int): List[Int] = {
    if (times <= 0) Nil
    else Gen.choose(0, 1).sample.get :: numberToList(times - 1)
  }

  // input generator
  val inputs: Gen[Data] = Gen.choose(3, 100).map(x => Solver.parseLines(getLines(x)))

  val numberSequences: Gen[List[Int]] = Gen.choose(5, 100).map(numberToList(_))

}
