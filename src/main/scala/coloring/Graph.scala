package coloring

/**
 * Created by Aleksey on 12/03/14.
 */

import scala.collection.mutable._
import scala.collection.mutable
import edu.princeton.cs.introcs.StdRandom


object Graph {

  type Edge = (Int, Int)

  def simple(V: Int, E: Int): Graph = {
    if (E > V * (V - 1) / 2) throw new IllegalArgumentException("Too many edges")
    if (E < 0) throw new IllegalArgumentException("Too few edges")

    val G = new Graph(V)
    val set = mutable.Set.empty[Edge]

    while (G.E < E) {
      val v = StdRandom.uniform(V)
      val w = StdRandom.uniform(V)
      val e = (v, w)

      if ((v != w) && !set.contains(e)) {
        set.add(e)
        G ++(v, w)
      }
    }

    G
  }
}

class Graph(val V: Int, var E: Int = 0) {

  import Graph.Edge

  private val adj: Array[MutableList[Int]] = Array.fill(V)(mutable.MutableList.empty)

  if (V < 0) throw new IllegalArgumentException("Number of vertices must be nonnegative")


  def ++(edge: Edge) {
    val v = edge._1
    val w = edge._2
    if (v < 0 || v >= V) throw new IndexOutOfBoundsException()
    if (w < 0 || w >= V) throw new IndexOutOfBoundsException()
    E += 1
    adj(v) += w
    adj(w) += v
  }


  /**
   * Returns the vertices adjacent to vertex <tt>v</tt>.
   * @return the vertices adjacent to vertex <tt>v</tt> as an Iterable
   * @param v the vertex
   * @throws java.lang.IndexOutOfBoundsException unless 0 <= v < V
   */
  def adjacent(v: Int): MutableList[Int] = {
    if (v < 0 || v >= V) throw new IndexOutOfBoundsException()
    adj(v)
  }

  //  public String toString() {
  //    StringBuilder s = new StringBuilder();
  //    String NEWLINE = System.getProperty("line.separator");
  //    s.append(V + " vertices, " + E + " edges " + NEWLINE);
  //    for (int v = 0;
  //    v < V;
  //    v ++)
  //    {
  //      s.append(v + ": ");
  //      for (int w: adj[v])
  //      {
  //        s.append(w + " ");
  //      }
  //      s.append(NEWLINE);
  //    }
  //    return s.toString();
  //  }


}