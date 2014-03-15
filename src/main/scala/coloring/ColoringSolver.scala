package coloring

/**
 * Created by Aleksey on 11/03/14.
 */
object ColoringSolver {

  // size, sequence of colors, objective value
  type Solution = (Int, Array[Int], Long)
  type Input = (Graph, Array[(Int, Int)])

  def main(args: Array[String]) {
    if (args.length < 1) {
      println( """
                 |This test requires an input file.
                 | Please select one from the data directory.(i.e.python solver.py./ data / gc_4_1)
                 | """)
    } else {
      val source = io.Source.fromFile(args(0))
      solveIt(source.getLines().map(_.split(" ").map(_.toInt)))

      source.close()
    }
  }

  def solveIt(iter: Iterator[Array[Int]]) {

    println(prepareSolution(solution(parseInput(iter))))
  }


  def parseInput(iter: Iterator[Array[Int]]): Graph = {
    val input = iter.next()
    val V = input(0)
    val E = input(1)
    val graph = new Graph(V)

    while (iter.nonEmpty) {
      val next = iter.next()
      graph ++(next(0), next(1))
    }

    graph
  }

  def prepareSolution(result: Solution): String = {
    val build: StringBuilder = new StringBuilder
    build ++= result._1.toString
    build ++= " 0\n"
    build ++= (result._2 mkString " ")

    build.toString()
  }

  def sortedByVertexLocality(graph: Graph): TraversableOnce[Int] = {
    (0 to graph.V - 1).map(x => (x, graph.adjacent(x).size)).sortBy(-_._2).map(_._1) // hack to sort desc
  }

  def solution(input: Graph): Solution = {
    new IterativeGreedySolve(input, sortedByVertexLocality(input)).solution
  }


}
