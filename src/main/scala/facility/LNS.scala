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
    val customerAssignments = Array.fill(M)(-1)

    var open: Set[Int] = customerAssignments.toSet

    //    println("start")

    var p = 0
    while (p < M) {
      if (customerAssignments(p) == -1) {

        //        print(", " + p)

        val wr = custIndex(p)(0)

        val customerNeighbourhood = (wareIndex(wr).iterator filter (customerAssignments(_) == -1) take 250).toArray

        if (customerNeighbourhood.length > 0) {
          val warehouseNeighbourhood = (customerNeighbourhood.foldLeft(immutable.Set(wr))((a, b) => a ++ custIndex(b)) - (-1)).toArray

          print("p = " + p + ", WRN = " + warehouseNeighbourhood.length + ", CSN = " + customerNeighbourhood.length)

          applySubSolution(customerNeighbourhood, warehouseNeighbourhood, customerAssignments, open)

          open = customerAssignments.toSet
        }
      }

      p += 1
    }

    p = 0
    while (p < M) {
      if (customerAssignments(p) == -1) {

        print("p = " + p)

        val customerNeighbourhood = Array.fill(1)(p)

        val warehouseNeighbourhood = custIndex(p)

        applySubSolution(customerNeighbourhood, warehouseNeighbourhood, customerAssignments, open)

        open = customerAssignments.toSet
      }

      p += 1
    }



    println("initial solution done")

    p = 0
    while (p < 500) {
      val wr = scala.util.Random.nextInt(N)

      val customerNeighbourhood = wareIndex(wr)

      val warehouseNeighbourhood = customerNeighbourhood.foldLeft(immutable.Set(wr))((a, b) => a ++ custIndex(b)).toArray

      print("p = " + p + ", WRN = " + warehouseNeighbourhood.length + ", CSN = " + customerNeighbourhood.length)

      applySubSolution(customerNeighbourhood, warehouseNeighbourhood, customerAssignments, open)

      open = customerAssignments.toSet
      p += 1
    }

    //    p = 0
    //    while (p < N) {
    //
    //      if (open.contains(p)) {
    //
    //        val customerNeighbourhood = wareIndex(p) take 50
    //
    //        //        val warehouseNeighbourhood = (customerNeighbourhood.foldLeft(immutable.Set(p))((a, b) => a ++ custIndex(b)) - (-1)).toArray
    //        val warehouseNeighbourhood = customerNeighbourhood.map(customerAssignments.apply).toSet.toArray
    //
    //        print("p = " + p + ", WRN = " + warehouseNeighbourhood.length + ", CSN = " + customerNeighbourhood.length)
    //
    //        // i know what i am doin
    //        applySubSolution(customerNeighbourhood, warehouseNeighbourhood, customerAssignments, open)
    //
    //        open = customerAssignments.toSet
    //
    //      }
    //      p += 1
    //    }

    (solutionValueFromAssignments(customerAssignments), customerAssignments)
  }

  def applySubSolution(customerNeighbourhood: Array[Int], warehouseNeighbourhood: Array[Int], customerAssignments: Array[Int], open: Set[Int]) = {
    val wareHouseCapacities = facilities map (_._2.toFloat)

    val solver = new MPSolver("", MPSolver.getSolverEnum("CBC_MIXED_INTEGER_PROGRAMMING"))
    solver.setTimeLimit(1000 * 60 * 6)

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
  }
}
