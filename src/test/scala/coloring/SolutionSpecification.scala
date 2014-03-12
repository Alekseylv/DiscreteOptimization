package coloring

/**
 * Created by Aleksey on 12/03/14.
 */

import org.scalacheck._
import Prop.forAll
import edu.princeton.cs.algs4.Graph

object SolutionSpecification extends Properties("Properties for valid solutions") with GraphGen {

  property("Solution must have the proper length") = forAll(graphs) {
    g: Graph =>
      val solution = ColoringSolver.solution(g)
      solution._2.size == g.V()
  }

  property("Solution must contain only specified amount of distinct colors") = forAll(graphs) {
    g: Graph =>
      val solution = ColoringSolver.solution(g)
      solution._2.toSet.size == solution._1
  }

  def distinctAdjecent(v: Int, result: Array[Int], graph: Graph): Boolean = {
    val iter = graph.adj(v).iterator()
    val vertexColor = result(v)
    var isTrue = true

    while (iter.hasNext && isTrue) {
      isTrue &&= result(iter.next()) != vertexColor
    }

    isTrue
  }

  property("Solution must be valid (no two adjacent vertices have same color)") = forAll(smallGraphs) {
    g: Graph =>
      val solution = ColoringSolver.solution(g)
      (0 to solution._2.size - 1).forall(x => distinctAdjecent(x, solution._2.toArray, g))
  }
}
