package tsp

/**
 * Created by Aleksey on 21/03/14.
 */
object TspSolver {

  type Node = (Float, Float)
  type Data = Array[Node]
  type Solution = (Double, TraversableOnce[Int])


  def main(args: Array[String]) {
    if (args.length < 1) {
      println( """
                 |This test requires an input file.
                 | Please select one from the data directory.(i.e.python solver.py./ data / gc_4_1)
                 | """)
    } else {
      val source = io.Source.fromFile(args(0))
      val input = source.getLines().toList
      val N = input.head.toInt
      val rest = input.tail.toList.map {
        x: String =>
          val p = x.split(" ").map(_.toFloat)
          (p(0), p(1))
      }

      println(prepareSolution(solve(N, rest.toArray)))

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

  def solve(N: Int, data: Data): Solution = {
    val list = 0 to N - 1
    (solutionValue(list, data), list)
  }

  def solutionValue(solution: TraversableOnce[Int], data: Data): Double = {
    var result: Double = 0
    val iter = solution.toIterator

    val first = iter.next()
    var current = iter.next()

    result += length(first, current, data)

    while (iter.hasNext) {
      val temp = iter.next()
      result += length(current, temp, data)
      current = temp
    }

    result += length(first, current, data)
    result
  }

  def length(i: Int, j: Int, data: Data): Double = {
    math.sqrt(sqr(data(i)._1 - data(j)._1) + sqr(data(i)._2 - data(j)._2))
  }

  def sqr(i: Double) = i * i

}
