package tsp

import java.io.IOException

import oscar.cp.CPModel
import oscar.cp.constraints.GraphPath
import oscar.cp.core.variables.{CPGraphVar, CPIntVar}

import scala.io.Source

/**
  * Created by Madhu on 8/5/17.
  */
object TSP extends CPModel with App{

  def getHamiltonianPath(numCities: Int, distanceMatrix: Array[Array[Double]])={

  }

  def getDistance(x: (Double,Double), y: (Double,Double)): Double = {
    math.sqrt(math.pow((y._1 - x._1),2) + math.pow((y._2 - x._2),2))
  }

  //override def main(args: Array[String]): Unit = {

    //try {


      val lines = Source.fromFile("/Users/Madhu/IdeaProjects/opt/src/main/resources/data/tsp/tsp_5_1").getLines().toVector

      val numCities = lines.take(1)(0).toInt

      val citiesCoordinates = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toDouble, x(1).toDouble)).zipWithIndex.map(x => (x._2,x._1)).toMap




      val distanceMatrix = Array.tabulate[Double](numCities,numCities)((x,y) => getDistance(citiesCoordinates.get(x).get,citiesCoordinates.get(y).get))

      distanceMatrix(0)(1)

      //val successor:Array[CPIntVar] = Array.fill(numCities)(CPIntVar(citiesCoordinates.keys))
      //val predecessor:Array[CPIntVar] = Array.fill(numCities)(CPIntVar(citiesCoordinates.keys))

      val totDist = CPIntVar(0 to distanceMatrix.flatten.sum.toInt)

      val graph = CPGraphVar(numCities+1)

      //add(new GraphPath(graph,0,nCities,dist,totDist))

      for (n <- 0 to numCities) add(graph.addNode(n))


/*
    }

    catch {
      case e: IOException => {
        println(e)

      }
    }


  }*/

}
