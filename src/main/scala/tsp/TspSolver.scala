package tsp

/**
 * Created by Aleksey on 21/03/14.
 */
object TspSolver {

  type Node = (Float, Float)
  type Data = Array[(Int, Node)]
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
      val rest = (0 to N - 1).zip(input.tail.toList.map {
        x: String =>
          val p = x.split(" ").map(_.toFloat)
          (p(0), p(1))
      })

      solve(N, rest.toArray)

      source.close()
    }
  }

  def solve(N: Int, data: Data): Solution = {
    (0, data map (x => x._1))
  }
}
