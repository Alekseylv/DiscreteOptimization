package coloring

import org.scalacheck._

/**
 * Created by Aleksey on 12/03/14.
 */
trait GraphGen {

  val arbInt = Gen.choose(10, 100)

  val graphs: Gen[Graph] = for {
    e <- Gen.choose(10, 100)
    v <- Gen.choose(e, e * (e - 1) / 2)
  } yield Graph.simple(e, v)

  val smallGraphs: Gen[Graph] = for {
    e <- Gen.choose(4, 20)
    v <- Gen.choose(e, e * (e - 1) / 2)
  } yield Graph.simple(e, v)
}
