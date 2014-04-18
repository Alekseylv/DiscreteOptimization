package facility

import com.google.ortools.linearsolver.MPVariable

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

      println(prepareSolution(new MIP(N, M, facilities.toArray, customers.toArray, args(0)).solution()))

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
}
