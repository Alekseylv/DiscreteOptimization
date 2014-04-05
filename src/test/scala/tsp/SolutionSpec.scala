package tsp

/**
 * Created by Aleksey on 21/03/14.
 */

import org.scalacheck._
import Prop.forAll

object SolutionSpec extends Properties("Specification for traveling salesmen problem solution") with DataGen {

  val fileName = "someRandomFileName.txt"

  def deleteFiles() {
    new java.io.File("cache" + java.io.File.separator + fileName).delete()
  }

  property("Solution must have proper length") = forAll(graphPlots) {
    x =>
      deleteFiles()
      TspSolver.solveIt(fileName, x._1, x._2)._2.toSet.size == x._1
  }

  property("Solution value must be higher than zero") = forAll(graphPlots) {
    x =>
      deleteFiles()
      TspSolver.solveIt(fileName, x._1, x._2)._1 > 0
  }

  property("Solution must contain only unique vertices in domain [0, N)") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solution = TspSolver.solveIt(fileName, x._1, x._2)
      isValid(solution._2, x._1)
  }

  property("Permutations should not contain edges passed as arguments") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))._1

      List((solver.seq(0), solver.seq(1)), (solver.seq(2), solver.seq(3)), (solver.seq(4),
        solver.seq(5)), (solver.seq(6), solver.seq(7))).forall(x => !perm.contains(x))

  }

  property("Permutations must have 4 edges") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))._1

      perm.length == 4
  }

  property("Permutations must contain vertices passed as arguments") = forAll(graphPlots) {
    x =>
      deleteFiles()
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

  property("Empty swap must result in the same arrays") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solutionSequence

      val perm = solver.doSwap(List(), Set())

      solver.indexMap.toList == perm._2.toList && solver.seq.toList == perm._1.toList
  }

  def isValid(solution: TraversableOnce[Int], N: Int) = {
    solution.toList.sorted == (0 to N - 1).toList
  }

  property("Applying permutation should result in expected value change") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solver = new TwoOpt(fileName, x._1, x._2)

      val solution = solver.solution

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))

      val oldEdgeLengths = List((solver.seq(0), solver.seq(1)), (solver.seq(2), solver.seq(3)), (solver.seq(4),
        solver.seq(5)), (solver.seq(6), solver.seq(7))).foldLeft(0.0)((a, b) => a + solver.length(b._1, b._2))

      val newSolution = solver.doSwap(perm._1, Set(solver.seq(0), solver.seq(2), solver.seq(4), solver.seq(6)))

      val actual = solver.solutionValue(newSolution._1)
      val newValue = solution._1 + perm._2 - oldEdgeLengths

      val valid = isValid(newSolution._1, x._1)

      valid && actual == newValue
  }

  property("Permutation should not contain invalid tours") = forAll(graphPlots) {
    x =>
      deleteFiles()
      val solver = new TwoOpt(fileName, x._1, x._2)

      solver.solution

      val perm = solver.bestPermutation(solver.seq(0), solver.seq(1), solver.seq(2), solver.seq(3), solver.seq(4),
        solver.seq(5), solver.seq(6), solver.seq(7))

      val f1 = perm._1.head._2
      val f2 = perm._1.tail.head._2

      List((solver.seq(2), solver.seq(3)), (solver.seq(4), solver.seq(5)), (solver.seq(6), solver.seq(7))) forall {
        y => !(f1 == y._1 && f2 == y._2) && !(f2 == y._1 && f1 == y._2)
      }
  }
}
