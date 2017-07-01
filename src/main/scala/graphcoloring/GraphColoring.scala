package graphcoloring

import java.io.IOException

import oscar.cp._
import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar

import scala.io.Source

/**
  * Created by msubbu on 6/16/17.
  */
object GraphColoring extends CPModel with App {

  override def main(args: Array[String]): Unit = {

    try {
      val lines = Source.fromFile(args(0)).getLines().toVector
      val firstLine = lines.take(1)(0).split(" ")

      val numNodes = firstLine(0).toInt
      val numEdges = firstLine(1).toInt

      val edges = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toInt, x(1).toInt))

      def hasEdge(x: Int, y: Int, edgeList: Vector[(Int, Int)]): Int = {
        if (edgeList.exists(k => (k._1, k._2) == (x, y)) || edgeList.exists(k => (k._1, k._2) == (y, x))) 1 else 0
      }

      hasEdge(3, 2, edges)

      println("# nodes: " + numNodes)
      println("# edges: " + numEdges)
      println("Edges: " + edges)

      def getAdjMatrix(edgeList: Vector[(Int, Int)]) = Array.tabulate(numNodes, numNodes)((x, y) => hasEdge(x, y, edgeList))

      val adjMatrix = getAdjMatrix(edges)

      println("Adjacency Matrix:")

      adjMatrix map (x => x.mkString(" ")) map println

      val num_colors = 5

      // variables
      val color:Array[CPIntVar] = Array.fill(numNodes)(CPIntVar(1 to num_colors))

      // constraints

      for (c1 <- 0 until numNodes;
           c2 <- 0 until c1 if adjMatrix(c1)(c2) == 1) {
        add(color(c1) !== color(c2))
      }
      // Symmetry breaking: first node has color 1
      add(color(0) === 1)

      search {
        binaryFirstFail(color)
      }
      onSolution {
        println("color:" + color.mkString(" "))
        val values = color map (x => x.value)
        println("distinct: "+values.distinct.length)
      }
      println(start(nSols=1))

    }

    catch {
      case e: IOException => {
        println(e)
        println("Problem processing the file. Expected format: ")
        println("numNodes numEdges")
        println("u_0 v_0")
        println("u_1 v_1")
        println("...")
        println("u_N-1 v_N-1")

      }
    }

  }

}


