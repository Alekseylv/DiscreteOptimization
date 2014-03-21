package tsp

import tsp.TspSolver.Data
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
    val set = new mutable.HashSet[(Float, Float)]()
    while (i < n) {
      val k = (random.nextInt(n).toFloat, random.nextInt(n).toFloat)
      if (!set.contains(k)) {
        set += k
        i += 1
      }
    }

    (0 to n - 1).zip(set).toArray
  }

  val graphPlots: Gen[(Int, Data)] = for {
    o <- Gen.choose(4, 20)
  } yield (o, randomInput(o))
}
