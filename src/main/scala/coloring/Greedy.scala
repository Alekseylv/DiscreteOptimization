package coloring

import scala.collection.{Set, mutable}
import scala.util.Random


/**
 * Created by Aleksey on 16/03/14.
 */
trait Greedy extends Solve {

  import scala.collection.immutable.IndexedSeq

  def map(): mutable.HashMap[Int, Int]

  def result(): Array[Int]

  def graph(): Graph

  def conflictNodes(i: Int): Set[Int] = {
    val result = this.result()
    graph.adjacent(i).filter(x => result(x) == result(i))
  }

  def vertexLocalityIndex(): Map[Int, IndexedSeq[Int]] = {
    val result = this.result()
    (0 to result.length - 1).groupBy(x => result(x))
  }

  def largestFirst(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.toList.sortBy(-_.length).flatten
  }

  def random(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    Random.shuffle(index.values).map(Random.shuffle(_)).flatten.toSeq
  }

  def reversed(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    index.values.map(_.reverse).flatten.toSeq
  }

  def heuristic(index: Map[Int, IndexedSeq[Int]]): Seq[Int] = {
    val probability = Random.nextInt(130)

    if (probability < 30) random(index)
    else if (probability < 80) reversed(index)
    else largestFirst(index)
  }
}
