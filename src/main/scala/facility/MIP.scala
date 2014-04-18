package facility

import com.google.ortools.linearsolver.MPSolver
import Solver._

/**
 * Created by Aleksey on 18/04/14.
 */
class MIP(val N: Int, val M: Int, val facilities: Array[Facility], val customers: Array[Customer]) extends ClosestNeighbours {

  def findVariableIndex(arr: Array[Int], to: Int): Int = {
    var i = 0
    while (i < arr.length) {
      if (arr(i) == to) return i
      i += 1
    }

    throw new Error("Could not find index")
  }

  def solution(name: String): Solution = {

    val solver = new MPSolver("IntegerProgrammingExample", MPSolver.getSolverEnum("CBC_MIXED_INTEGER_PROGRAMMING"))
    assert(solver != null)

    val (wareIndex, custIndex) = getIndex(name)

    assert(wareIndex.length == N)
    assert(custIndex.length == M)

    val warehouses = solver.makeBoolVarArray(N)
    val assignment = Array.fill(M)(solver.makeBoolVarArray(size))

    val inf = MPSolver.infinity()

    // redundant
    val demandConstraint = solver.makeConstraint(customers.foldRight(0.0)(_._1 + _), inf)

    //minimize
    var i = 0
    while (i < N) {
      demandConstraint.setCoefficient(warehouses(i), facilities(i)._2)
      solver.objective().setCoefficient(warehouses(i), facilities(i)._1)

      val capacity = solver.makeConstraint(0, facilities(i)._2)

      wareIndex(i) foreach { x =>
        val index = findVariableIndex(custIndex(x), i)
        capacity.setCoefficient(assignment(x)(index), customers(x)._1)

      }

      i += 1
    }

    i = 0
    while (i < M) {
      var j = 0
      val global = solver.makeConstraint(1, 1)

      while (j < size) {
        //set objective
        solver.objective().setCoefficient(assignment(i)(j), length(facilities(custIndex(i)(j)), customers(i)))

        //set constraints
        val c = solver.makeConstraint(0, inf)
        c.setCoefficient(warehouses(custIndex(i)(j)), 1)
        c.setCoefficient(assignment(i)(j), -1)

        global.setCoefficient(assignment(i)(j), 1)

        j += 1
      }
      i += 1
    }

    solver.setTimeLimit(1000 * 60 * 10)
    val status = solver.solve()

    if (status == MPSolver.ABNORMAL || status == MPSolver.UNBOUNDED || status == MPSolver.INFEASIBLE) throw new Error("Bad model")

    //        println(solver.wallTime())
    //   println(warehouses.map(_.solutionValue()).toList)

    (solver.objective().value(), (0 to assignment.length - 1) map (fx => custIndex(fx)(findTrue(assignment(fx)))))
  }
}
