package graphcoloring

import java.io.IOException

import oscar.cp._
import oscar.cp.core.variables.CPIntVar

import scala.io.Source

/**
  * Created by msubbu on 6/16/17.
  */
object GraphColoring extends CPModel with App {

  /**
    * Helper function for adjacency matrix builder. Returns 1 if x and y are neighbors
    * @param x
    * @param y
    * @param edgeList
    * @return
    */
  def hasEdge(x: Int, y: Int, edgeList: Vector[(Int, Int)]): Int = {
    if (edgeList.exists(k => (k._1, k._2) == (x, y)) || edgeList.exists(k => (k._1, k._2) == (y, x))) 1 else 0
  }

  /**
    * turns an edge list into adjacency matrix
    * @param edgeList
    * @return
    */

  def getAdjMatrix(numNodes: Int, edgeList: Vector[(Int, Int)]):Array[Array[Int]] = Array.tabulate(numNodes, numNodes)((x, y) => hasEdge(x, y, edgeList))

  def edgesSortedbyDegree(aMatrix: Array[Array[Int]]):Array[(Int, Int)] = aMatrix.zipWithIndex map (x => (x._2,x._1.sum)) sortWith(_._2 > _._2)

  /**
    * get the first feasible solution for this chromatic number
    * @param numColors
    * @param numNodes
    * @param maxDegreeNode
    * @param adjMatrix
    */
  def getFirstSoln(numColors: Int, numNodes: Int, maxDegreeNode:Int, adjMatrix:Array[Array[Int]])= {

      val color: Array[CPIntVar] = Array.fill(numNodes)(CPIntVar(0 to numColors - 1))

      // constraints

      for (c1 <- 0 until numNodes;
           c2 <- 0 until c1 if adjMatrix(c1)(c2) == 1) {
        add(allDifferent(color(c1), color(c2)))
      }
      // most constrained node gets a color first
      add(color(maxDegreeNode) === 0)

      search {
        binaryFirstFail(color)

      }
      onSolution {
        println("color:" + color.mkString(" "))
        val values = color map (x => x.value)
        println("Chromatic number: "+values.distinct.length)
      }
      println(start(nSols = 1))
    }


  override def main(args: Array[String]): Unit = {

    try {
      val lines = Source.fromFile(args(0)).getLines().toVector
      val firstLine = lines.take(1)(0).split(" ")

      val numNodes = firstLine(0).toInt
      val numEdges = firstLine(1).toInt

      val edges = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toInt, x(1).toInt))

      println("computing adjacency matrix:")
      val adjMatrix = getAdjMatrix(numNodes, edges)

      println("find node with max degree:")
      val edgesByDegree = edgesSortedbyDegree(adjMatrix)

      val maxDegreeNode = edgesByDegree(0)._1
      val maxDegree = edgesByDegree(0)._2


      println("# nodes: " + numNodes)
      println("# edges: " + numEdges)
      println("Edges: " + edges)
      println("Adjacency Matrix:")

      adjMatrix map (x => x.mkString(" ")) map println

      println("\n")
      println("Max Degree: " + maxDegree)


      // assume the worst
      // TODO: Optimize to find the chromatic number
      val chromaticNumber: Int = 16


        println("Trying # colors: " + chromaticNumber)
        println("*******************************************")

       getFirstSoln(chromaticNumber, numNodes, maxDegreeNode, adjMatrix)


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


