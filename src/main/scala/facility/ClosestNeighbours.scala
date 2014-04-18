package facility

import java.io.File
import Solver._
import scala.collection.mutable

/**
 * Created by Aleksey on 18/04/14.
 */
trait ClosestNeighbours {

  def N: Int

  def M: Int

  def facilities: Array[Facility]

  def customers: Array[Customer]

  val size = 30

  def getIndex(name: String): (Array[Set[Int]], Array[Set[Int]]) = {
    val file = new File("cache/" + name)

    if (file.exists()) readAndParse(file)
    else createAndCompute(file)
  }

  def createAndCompute(file: File): (Array[Set[Int]], Array[Set[Int]]) = {
    file.getParentFile.mkdirs()
    file.createNewFile()

    val warehouses = Array.fill(N)(new mutable.HashSet[Int]())

    val sorted = (0 to M - 1) map { x =>
      val result = ((0 to N - 1).sortBy(p => length(facilities(p), customers(x))) take size).toSet
      result foreach (p => warehouses(p) += x)
      result
    }

    val first = warehouses map (_ mkString "") mkString "\n"
    val second = sorted map (_ mkString " ") mkString "\n"
    val out = new java.io.PrintWriter(file)

    try {
      out write first
      out write second
    }
    finally out.close()

    (warehouses map (_.toSet), sorted.toArray)
  }

  def readAndParse(file: File): (Array[Set[Int]], Array[Set[Int]]) = {

    val input = io.Source.fromFile(file).getLines().map(x => (x split " " map (_.toInt)).toSet).toList.splitAt(N)

    (input._1.toArray, input._2.toArray)
  }
}
