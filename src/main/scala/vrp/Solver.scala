package vrp

import scala.collection.mutable

/**
 * Created by Aleksey on 25/04/14.
 */
object Solver {

  type Data = Array[(Int, Float, Float)]

  type Node = (Int, Int, Double)

  type Point = (Float, Float)

  var zero = (0, 0): Point

  val heap = new mutable.PriorityQueue[Node]()(Ordering.by(-_._3))

  @inline implicit def IntToPoint(node: Int)(implicit data: Data) = {
    (data(node)._2, data(node)._3)
  }

  def main(args: Array[String]) {
    if (args.length < 1) {
      println( """
                 |This test requires an input file.
                 | Please select one from the data directory.(i.e.python solver.py./data/vrp_5_4_1)
                 | """)
    } else {
      val source = io.Source.fromFile(args(0))
      val lines = source.getLines()
      val firstLine = lines.next().split(" ").map(_.toInt)
      val N = firstLine(0) - 1
      val V = firstLine(1) - 1
      val cap = firstLine(2)

      val parsed = lines.next().split(" ")
      zero = (parsed(1).toFloat, parsed(2).toFloat)

      implicit val data: Data = lines.map({ x => val s = x.split(" "); (s(0).toInt, s(1).toFloat, s(2).toFloat)}).toArray

      var i = 0
      while (i < N - 1) {
        var j = i + 1
        while (j < N) {
          heap.enqueue(savings(i, j))
          j += 1
        }

        i += 1
      }

      val routes: mutable.HashSet[Vector[Int]] = (0 to N - 1).map(Vector(_)).to[mutable.HashSet]
      val map = mutable.HashMap[Int, (Int, Vector[Int])]()
      routes.foreach(x => map += ((x.head, (data(x.head)._1, x))))

      var min: Node = null
      do {
        min = heap.dequeue()

        if (min._3 > 0 || routes.size > V) {
          val first = map.get(min._1)
          val second = map.get(min._2)

          if (first.isDefined && second.isDefined && first.get != second.get && first.get._1 + second.get._1 < cap) {
            val r1 = first.get._2
            val r2 = second.get._2
            routes -= r1
            routes -= r2
            map -= min._1
            map -= min._2

            val melded = if (r1.last == min._1 && r2.head == min._2) {
              r1 ++ r2
            } else if (r1.head == min._1 && r2.last == min._2) {
              r2 ++ r1
            } else if (r1.last == min._1 && r2.last == min._2) {
              r1 ++ r2.reverse
            } else if (r1.head == min._1 && r2.head == min._2) {
              r1.reverse ++ r2
            } else throw new Error("You failed at life bro")

            routes += melded
            map(melded.head) = (first.get._1 + second.get._1, melded)
            map(melded.last) = (first.get._1 + second.get._1, melded)
          }
        }

      } while (!heap.isEmpty && (min._3 > 0 || routes.size > V))

      val solutionValue = routes.map({ x =>
        var result = 0.0
        x.tail.foldLeft(x.head) { (a, b) => result += length(a, b); b}
        result + length(zero, x.head) + length(zero, x.last)
      }).fold(0.0)(_ + _)

      val resultRoutes = routes map (x => x.map(_ + 1).+:(0).:+(0))

      println(prepareSolution(solutionValue, resultRoutes))
    }
  }

  def prepareSolution(value: Double, resultRoutes: mutable.HashSet[Vector[Int]]): String = {
    val build: StringBuilder = new StringBuilder
    build ++= value.toString
    build ++= " 0\n"
    build ++= resultRoutes map (_ mkString " ") mkString "\n"

    build.toString()
  }

  def savings(i: Int, j: Int)(implicit data: Data): Node = {
    (i, j, length(i, zero) + length(j, zero) - length(i, j))
  }

  def length(p1: Point, p2: Point) = {
    math.sqrt(sqr(p1._1 - p2._1) + sqr(p1._2 - p2._2))
  }

  def sqr(x: Float) = x * x
}
