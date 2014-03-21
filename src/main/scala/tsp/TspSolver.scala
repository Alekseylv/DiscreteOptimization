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

      println(prepareSolution(solveIt(N, rest.toArray)))

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

  def solveIt(n: Int, input: Data): Solution = {
    new GreedySolve(n, input).solution
  }

}
