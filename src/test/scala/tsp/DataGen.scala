package tsp

import tsp.TspSolver.{Node, Data}
import org.scalacheck.Gen
import scala.collection.mutable
import scala.util.Random

/**
 * Created by Aleksey on 21/03/14.
 */
trait DataGen {

  def randomInput(n: Int): Data = {

    val random = new Random()
    var i = 0
    val set = new mutable.HashSet[(Double, Double)]()
    while (i < n) {
      val k = (random.nextInt(n).toDouble, random.nextInt(n).toDouble)
      if (!set.contains(k)) {
        set += k
        i += 1
      }
    }

    set.toArray
  }

  val graphPlots: Gen[(Int, Data)] = for {
    o <- Gen.choose(4, 20)
  } yield (o, randomInput(o))

  val intersectedLines: Gen[(Node, Node, Node, Node)] = for {
    x <- Gen.choose(0.0, 100000.0)
    y <- Gen.choose(0.0, 100000.0)
    k <- Gen.choose(0.0, 100000.0)
  } yield ((x, y), (x + k, y + k), (x + k, y), (x, y + k))

}
