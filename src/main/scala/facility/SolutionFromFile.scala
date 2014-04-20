package facility

/**
 * Created by Aleksey on 20/04/14.
 */
object SolutionFromFile {
  def main(args: Array[String]) {
    println(io.Source.fromFile("solution8").getLines() mkString "\n")
  }
}
