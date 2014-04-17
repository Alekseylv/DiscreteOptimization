package facility

import com.google.ortools.linearsolver.{MPVariable, MPSolver}

/**
 * Created by Aleksey on 16/04/14.
 */
object Solver {

  System.loadLibrary("jniortools")

  // (Cost, Capacity, X, Y)
  type Facility = (Float, Int, Float, Float)

  // (Demand, X, Y)
  type Customer = (Float, Float, Float)

  type Solution = (Double, TraversableOnce[Int])

  def main(args: Array[String]) {
    if (args.length < 1) {
      println( """
                 |This test requires an input file.
                 | Please select one from the data directory.(i.e.python solver.py./ data / fl_3_1)
                 | """)
    } else {
      val source = io.Source.fromFile(args(0))
      val input = source.getLines().toList
      val arr = input.head.split(" ").map(_.toInt)
      val N = arr(0)
      val M = arr(1)

      val rest = input.tail.splitAt(N)

      val facilities = rest._1.map({ x =>
        val p = x.split(" ")
        (p(0).toFloat, p(1).toInt, p(2).toFloat, p(3).toFloat)
      })

      val customers = rest._2.map({ x =>
        val p = x.split(" ")
        (p(0).toFloat, p(1).toFloat, p(2).toFloat)
      })

      assert(facilities.length == N)
      assert(customers.length == M)

      println(prepareSolution(solveIt(N, M, facilities.toArray, customers.toArray)))

      source.close()
    }
  }

  def prepareSolution(result: Solution): String = {
    val build: StringBuilder = new StringBuilder
    build ++= result._1.toString
    build ++= " 0\n"
    build ++= (result._2 mkString " ")

    build.toString()
  }

  def length(warehouse: Facility, customer: Customer) = {
    math.sqrt(sqr(warehouse._3 - customer._2) + sqr(warehouse._4 - customer._3))
  }

  def sqr(x: Float) = x * x

  def findTrue(arr: Array[MPVariable]): Int = {
    var i = 0
    while (i < arr.length) {
      if (arr(i).solutionValue() == 1.0) return i
      i += 1
    }

    throw new Error("Customer not assigned to a facility")
  }

  def solveIt(N: Int, M: Int, facilities: Array[Facility], customers: Array[Customer]): Solution = {

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
        capacity.setCoefficient(assignment(j)(i), 1)
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

    //   println(warehouses.map(_.solutionValue()).toList)

    (solver.objective().value(), assignment map findTrue)
  }
}
