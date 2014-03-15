package coloring

import org.scalacheck._

/**
 * Created by Aleksey on 12/03/14.
 */
trait GraphGen {

  val arbInt = Gen.choose(10, 100)

  def randomGraph(from: Int, to: Int): Gen[Graph] = for {
    e <- Gen.choose(from, to)
    v <- Gen.choose(e, e * (e - 1) / 2)
  } yield Graph.simple(e, v)

  val graphs = randomGraph(10, 100)

  val smallGraphs = randomGraph(4, 20)

}
