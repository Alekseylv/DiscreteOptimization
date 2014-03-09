package knapsack

import java.io.File
import scala.util.control.NonFatal


/**
 * Created by Aleksey on 07/03/14.
 */
object Solver {

  // (Index, Value, Weight, Value/Weight)
  type Data = Array[(Int, Int, Int, Double)]

  def solveIt(lines: List[String]) {
    val firstLine = lines(0).split(" ")
    val numItems = firstLine(0).toInt
    val capacity = firstLine(1).toInt

    val data = parseLines(lines.tail)
    val table = new DPTable(data, numItems, capacity)
    val (seq, value) = table getValue(capacity, numItems)

    val zipped = (decompress(seq), data.toList.tail).zipped.toList.sortBy(_._2._1).map(_._1)

    println(prepareSolution(zipped, value))
  }


  // greedy algorithm for a crude estimate (belgian chocolate algorithm)
  def estimate(from: Int, data: Data, capacity: Int) = {
    var value: Int = 0
    var weight: Int = 0
    var i:Int = from - 1

    while (weight <= capacity && i > -1) {
      if (data(i)._3 + weight <= capacity) {
        value += data(i)._2
        weight += data(i)._3
      } else {
        value += Math.ceil(data(i)._4 * (capacity - weight)).toInt
        weight += capacity - weight
      }
      i -= 1
    }

    (value, weight)
  }

  def parseLines(lines: List[String]): Data = {

    var i = 0
    val data = lines map (x => {
      val parts = x.split(" ")
      i += 1
      (i, parts(0).toInt, parts(1).toInt, parts(0).toInt / parts(1).toDouble)
    })

    ((0, 0, 0, 0.0) :: data.sortBy(_._4)).toArray
  }

  def main(args: Array[String]) {
    if (args.length < 1) {
      println( """
                 |This test requires an input file.
                 | Please select one from the data directory.(i.e.python solver.py./ data / ks_4_0)
                 | """)
    } else {
      val source = io.Source.fromFile(args(0))
      solveIt(source.getLines().toList)

      source.close()
    }
  }

  def addDigit(digit: Int, sequence: List[Int]): List[Int] = {
    if (sequence.isEmpty) List(1, digit)
    else if (sequence.tail.head == digit) (sequence.head + 1) :: sequence.tail
    else 1 :: digit :: sequence
  }

  def decompress(sequence: List[Int]): List[Int] = {
    def digitToList(digit: Int, times: Int, seq: List[Int]): List[Int] = {
      if (times == 0) decompressHelper(seq)
      else digit :: digitToList(digit, times - 1, seq)
    }

    def decompressHelper(sequence: List[Int]): List[Int] = {
      if (sequence.isEmpty) Nil
      else digitToList(sequence.tail.head, sequence.head, sequence.tail.tail)
    }

    decompressHelper(sequence).reverse
  }

  def prepareSolution(taken: List[Int], value: Int): String = {
    val builder = new StringBuilder
    builder ++= value.toString
    builder ++= " 0\n"
    builder ++= (taken mkString " ")

    builder.toString
  }


}
