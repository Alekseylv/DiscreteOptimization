package tsp

/**
 * Created by Aleksey on 21/03/14.
 */

import org.scalacheck._
import Prop.forAll

object SolutionSpec extends Properties("Specification for traveling salesmen problem solution") with DataGen {

  val fileName = "someRandomFileName.txt"

  property("Solution must have proper length") = forAll(graphPlots) {
    x =>

      val result = TspSolver.solveIt(fileName, x._1, x._2)._2.toSet.size == x._1
      new java.io.File(fileName).delete()
      result
  }

  property("Solution value must be higher than zero") = forAll(graphPlots) {
    x =>
      val result = TspSolver.solveIt(fileName, x._1, x._2)._1 > 0
      new java.io.File(fileName).delete()
      result
  }
}
