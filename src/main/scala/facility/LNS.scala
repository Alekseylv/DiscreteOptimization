package facility

/**
 * Created by Aleksey on 19/04/14.
 */

import facility.Solver._
import scala.collection.immutable
import com.google.ortools.linearsolver.{MPVariable, MPSolver}


class LNS(N: Int, M: Int, facilities: Array[Facility], customers: Array[Customer], name: String)
  extends MIP(N, M, facilities, customers, name) {

  def solutionValueFromAssignments(customerAssignments: Array[Int]) = {
    (0 to M - 1).foldLeft(0.0)((a, b) => a + length(facilities(customerAssignments(b)), customers(b))) +
      customerAssignments.toSet.foldLeft(0.0)(_ + facilities(_)._1)
  }

  def subSolutionValue(solver: MPSolver, customerVariables: Array[Array[MPVariable]]) = {
    (0 to customerVariables.length - 1) map (fx => findTrue(customerVariables(fx)))
  }

  override def solution(): (Double, TraversableOnce[Int]) = {
    val solution = super.solution()
    val customerAssignments = solution._2.toArray

    var open: Set[Int] = customerAssignments.toSet
    var p = 0
    while (p < N) {

      if (open.contains(p)) {

        val customerNeighbourhood = wareIndex(p)

        val warehouseNeighbourhood = (wareIndex(p).foldLeft(immutable.Set(p))((a, b) => a ++ custIndex(b)) - (-1)).toArray

        val wareHouseCapacities = facilities map (_._2.toFloat)

        val solver = new MPSolver(p.toString, MPSolver.getSolverEnum("CBC_MIXED_INTEGER_PROGRAMMING"))

        val warehouseVariables = solver.makeBoolVarArray(warehouseNeighbourhood.length)
        val customerVariables = Array.fill(customerNeighbourhood.length)(solver.makeBoolVarArray(warehouseNeighbourhood.length))

        (0 to M - 1) foreach { custmr =>
          if (!customerNeighbourhood.contains(custmr) && customerAssignments(custmr) != -1) {
            wareHouseCapacities(customerAssignments(custmr)) -= customers(custmr)._1
          }
        }

        //build a subproblem
        //warehouse constraints/objective
        var i = 0
        while (i < warehouseNeighbourhood.length) {
          val wr = warehouseNeighbourhood(i)
          solver.objective().setCoefficient(warehouseVariables(i), facilities(wr)._1)

          val capacity = solver.makeConstraint(0, wareHouseCapacities(wr))

          var j = 0
          while (j < customerNeighbourhood.length) {
            capacity.setCoefficient(customerVariables(j)(i), customers(customerNeighbourhood(j))._1)
            j += 1
          }

          i += 1
        }

        //customer constraints/objective
        i = 0
        while (i < customerNeighbourhood.length) {
          var j = 0
          val custmr = customerNeighbourhood(i)
          val global = solver.makeConstraint(1, 1)

          while (j < warehouseNeighbourhood.length) {
            val wr = warehouseNeighbourhood(j)

            //set objective
            solver.objective().setCoefficient(customerVariables(i)(j), length(facilities(wr), customers(custmr)))

            //set constraints
            global.setCoefficient(customerVariables(i)(j), 1)

            val c = solver.makeConstraint(0, inf)
            c.setCoefficient(warehouseVariables(j), 1)
            c.setCoefficient(customerVariables(i)(j), -1)


            j += 1
          }
          i += 1
        }

        //additional constraints for open warehouses
        i = 0
        while (i < warehouseNeighbourhood.length) {
          if (open.contains(warehouseNeighbourhood(i))) {
            solver.makeConstraint(1, 1).setCoefficient(warehouseVariables(i), 1)
          }
          i += 1
        }

        val result = subSolutionValue(solve(solver), customerVariables)

        i = 0
        while (i < result.length) {
          customerAssignments(customerNeighbourhood(i)) = warehouseNeighbourhood(result(i))
          i += 1
        }
        open = customerAssignments.toSet

      }
      p += 1
    }

    (solutionValueFromAssignments(customerAssignments), customerAssignments)
  }
}
