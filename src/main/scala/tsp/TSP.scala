package tsp

import java.io.IOException

import oscar.cp._
import oscar.cp.constraints.GraphPath
import oscar.cp.core.variables.{CPGraphVar, CPIntVar}
import oscar.visual._

import scala.io.Source

/**
  * Created by Madhu on 8/5/17.
  */
object TSP extends CPModel with App{

  def getHamiltonianPath(numCities: Int, distanceMatrix: Array[Array[Double]])={

  }

  def getDistance(x: (Double,Double), y: (Double,Double)): Int = {
    math.sqrt(math.pow((y._1 - x._1),2) + math.pow((y._2 - x._2),2)).ceil.toInt
  }

  def getDistanceInDouble(x: (Double,Double), y: (Double,Double)): Double = {
    math.sqrt(math.pow((y._1 - x._1),2) + math.pow((y._2 - x._2),2))
  }

   override def main(args: Array[String]): Unit = {

     //val lines = Source.fromFile("/Users/Madhu/IdeaProjects/opt/src/main/resources/data/tsp/tsp_5_1").getLines().toVector
     val lines = Source.fromFile("/Users/msubbu/Documents/git/opt/src/main/resources/data/tsp/tsp_51_1").getLines().toVector

     val numCities = lines.take(1)(0).toInt

     val citiesCoordinates = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toDouble, x(1).toDouble)).zipWithIndex.map(x => (x._2, x._1)).toMap

     val distanceMatrix = Array.tabulate[Int](numCities, numCities)((x, y) => getDistance(citiesCoordinates.get(x).get, citiesCoordinates.get(y).get))

     def dist(n1: Int, n2: Int): Int = distanceMatrix(n1)(n2) toInt

     println(numCities)

     val succ = Array.fill(numCities)(CPIntVar(0 until numCities))
     val totDist = CPIntVar(0 to distanceMatrix.flatten.sum.toInt)

     add(circuit(succ), Strong)
     add(minAssignment(succ, distanceMatrix, totDist))
     add(sum(0 until numCities)(i => distanceMatrix(i)(succ(i))) == totDist)

     //minimize(totDist) search binaryStatic(succ)
     minimize(totDist) search binaryFirstFail(succ)

     var nSols = 0
     onSolution {
       nSols += 1

       println("Distance: " + totDist.value)
       println(succ mkString (" "))
     }
     val stats = start()
     println(stats)

   }
}
