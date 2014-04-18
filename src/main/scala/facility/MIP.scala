package facility

import com.google.ortools.linearsolver.MPSolver
import Solver._

/**
 * Created by Aleksey on 18/04/14.
 */
class MIP(val N: Int, val M: Int, val facilities: Array[Facility], val customers: Array[Customer]) extends ClosestNeighbours {

  def solution(name: String): Solution = {

    val solver = new MPSolver("IntegerProgrammingExample", MPSolver.getSolverEnum("CBC_MIXED_INTEGER_PROGRAMMING"))
    assert(solver != null)

    val warehouses = solver.makeBoolVarArray(N)
    val assignment = Array.fill(M)(solver.makeBoolVarArray(N))

    val inf = MPSolver.infinity()

    //minimize
    var i = 0
    while (i < N) {
      solver.objective().setCoefficient(warehouses(i), facilities(i)._1)

      val capacity = solver.makeConstraint(0, facilities(i)._2)
      var j = 0
      while (j < M) {
        capacity.setCoefficient(assignment(j)(i), customers(j)._1)
        j += 1
      }

      i += 1
    }

    i = 0
    while (i < M) {
      var j = 0
      val global = solver.makeConstraint(1, 1)

      while (j < N) {
        //set objective
        solver.objective().setCoefficient(assignment(i)(j), length(facilities(j), customers(i)))

        //set constraints
        val c = solver.makeConstraint(0, inf)
        c.setCoefficient(warehouses(j), 1)
        c.setCoefficient(assignment(i)(j), -1)

        global.setCoefficient(assignment(i)(j), 1)

        j += 1
      }
      i += 1
    }

    val status = solver.solve()
    if (status == MPSolver.ABNORMAL || status == MPSolver.UNBOUNDED || status == MPSolver.INFEASIBLE) throw new Error("Bad model")

    //    println(solver.wallTime())
    //   println(warehouses.map(_.solutionValue()).toList)

    (solver.objective().value(), assignment map findTrue)
  }
}
