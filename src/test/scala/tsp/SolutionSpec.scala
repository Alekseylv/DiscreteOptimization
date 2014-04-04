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
      new java.io.File("cache/" + fileName).delete()
      TspSolver.solveIt(fileName, x._1, x._2)._2.toSet.size == x._1
  }

  property("Solution value must be higher than zero") = forAll(graphPlots) {
    x =>
      new java.io.File("cache/" + fileName).delete()
      TspSolver.solveIt(fileName, x._1, x._2)._1 > 0
  }

  property("Permutations should not contain certain edges passed as arguments") = forAll(graphPlots) {
    x =>
      new java.io.File("cache/" + fileName).delete()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))._1

      List((solver.seq(0), solver.seq(1)), (solver.seq(2), solver.seq(3)), (solver.seq(4),
        solver.seq(5)), (solver.seq(6), solver.seq(7))).forall {
        x =>
          !perm.contains(x)
      }

  }

  property("Permutations must have 4 edges") = forAll(graphPlots) {
    x =>
      new java.io.File("cache/" + fileName).delete()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))._1

      perm.length == 4
  }

  property("Permutations must contain vertices passed as arguments") = forAll(graphPlots) {
    x =>
      new java.io.File("cache/" + fileName).delete()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))._1

      val result = perm.flatMap({
        x => List(x._1, x._2)
      }).toList.sorted
      val other = List(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7)).sorted

      result == other
  }
}
