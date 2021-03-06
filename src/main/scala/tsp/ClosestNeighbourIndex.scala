package tsp

import tsp.TspSolver.Data
import java.io.File


/**
 * Created by Aleksey on 27/03/14.
 */
trait ClosestNeighbourIndex {

  def N: Int

  def data: Data

  val size = 20

  def getIndex(name: String): Array[Set[Int]] = {
    val file = new File("cache/" + name)

    if (file.exists()) readAndParse(file)
    else createAndCompute(file)
  }

  def createAndCompute(file: File): Array[Set[Int]] = {
    file.getParentFile.mkdirs()

    val sorted = (0 to N - 1) map { x =>
      ((0 to N - 1).filter(x != _).sortBy(length(x, _)) take size).toSet
    }

    val string = sorted map (_ mkString " ") mkString "\n"
    val out = new java.io.PrintWriter(file.getName)

    try out write string
    finally out.close()

    sorted.toArray
  }

  def readAndParse(file: File): Array[Set[Int]] = {

    (io.Source.fromFile(file).getLines() map {
      x: String => x.split(" ").map(_.toInt).toSet
    }).toArray
  }

  def length(i: Int, j: Int): Double = {
    math.sqrt(sqr(data(i)._1 - data(j)._1) + sqr(data(i)._2 - data(j)._2))
  }

  def sqr(i: Double) = i * i


}
