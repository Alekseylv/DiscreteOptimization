package knapsack

import java.util
import scala.collection.mutable._
import Solver.Data

/**
 * Created by Aleksey on 08/03/14.
 */
class DPTable(val data: Data, val items: Int, val capacity: Int) {

  // (Capacity, ItemNum)
  type Key = (Int, Int)
  type Value = (List[Int], Int)
  var estimate = 0
  val empty: Value = (Nil, 0)


  val table: util.HashMap[Key, Value] = new util.HashMap(items * 2)

  def getValue(key: Key): Value = getValue(key, 0)

  private def getValue(key: Key, currentValue: Int): Value = {
    if (!table.containsKey(key)) evalCell(key, currentValue)
    else table.get(key)
  }

  // the meat of solver
  private def evalCell(key: Key, currentValue: Int): Value = {
    if (key._2 == 0) {
      if (currentValue > estimate) {
        estimate = currentValue
      }
      table.put(key, empty)
      empty
    } else if (data(key._2)._3 <= key._1) {

      // pruning by branch and bound
      if (currentValue > estimate) estimate = currentValue
      else if (Solver.estimate(key._2, data, key._1)._1 + currentValue < estimate) {
        return empty
      }

      //TODO replace by expression?
      val weight = key._1 - data(key._2)._3
      val (a, b) = getValue((weight, key._2 - 1), currentValue + data(key._2)._2)
      val first = (Solver.addDigit(1, a), b + data(key._2)._2)

      val (x, y) = getValue((key._1, key._2 - 1), currentValue)
      val second = (Solver.addDigit(0, x), y)

      if (second._2 > first._2) {
        table.put(key, second)
        second
      } else {
        table.put(key, first)
        first
      }

    } else {

      val (x, y) = getValue((key._1, key._2 - 1), currentValue)
      val second = (Solver.addDigit(0, x), y)

      table.put(key, second)
      second
    }
  }


  // useful for small tables
  override def toString = {
    val iter = table.entrySet().iterator()
    val builder: StringBuilder = new StringBuilder()

    while (iter.hasNext) {
      builder ++= iter.next().toString
    }

    builder.toString()
  }

}
