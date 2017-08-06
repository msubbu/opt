package tsp

import oscar.cp._
import oscar.cp.constraints.GraphPath
import oscar.cp.{CPModel, Strong, add, binaryFirstFail, circuit, minAssignment, minimize, onSolution, start, sum}
import oscar.cp.core.variables.{CPGraphVar, CPIntVar}
import tsp.TSP.getDistance

import scala.io.Source

/**
  * Created by msubbu on 8/5/17.
  */

object TSPGraph extends CPModel with App {
  val lines = Source.fromFile("/Users/msubbu/Documents/git/opt/src/main/resources/data/tsp/tsp_51_1").getLines().toVector

  val numCities = lines.take(1)(0).toInt

  val citiesCoordinates = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toDouble, x(1).toDouble)).zipWithIndex.map(x => (x._2, x._1)).toMap

  val distanceMatrix = Array.tabulate[Int](numCities, numCities)((x, y) => getDistance(citiesCoordinates.get(x).get, citiesCoordinates.get(y).get))

  def dist(n1 : Int, n2 : Int) : Int = distanceMatrix(n1)(n2) toInt

  println(numCities)

       val maxDist = distanceMatrix.flatten.sum.toInt
       val totDist = CPIntVar(0 to maxDist)
       val graph = CPGraphVar(numCities)
       graph.removeEdge(0, numCities)

       for (n <- 0 to numCities)
        add(graph.addNode(n))

    add(new GraphPath(graph,0,numCities,dist,totDist))

       minimize(totDist) search {
         val requiredEdges : List[Int] =
           graph.requiredNodes.flatMap(graph.requiredEdges(_))
         val possibleNotRequiredEdges : List[Int] =
           graph.possibleNodes.flatMap(graph.possibleEdges(_)).filter(n
           => !requiredEdges.contains(n))
         if (possibleNotRequiredEdges.isEmpty){
           noAlternative
         }
         else {
           val e = possibleNotRequiredEdges.head
           val (s,d) = graph.edge(e)
           branch { add(graph.addEdge(s,d)) } { add(graph.removeEdge(s,d)) }
         }
       }

       var nSols = 0
       onSolution {
         nSols += 1
         var arr = Array.fill(numCities)(0)
         for (i <- 0 until numCities)
           arr(i) = graph.edge(graph.possibleOutEdges(i).head)._2

         println("# Solutions: "+ nSols)
         println("# Total Distance: "+ totDist.value)
         println("# Path: "+ arr)

       }
       val stats = start()
       println(stats)


}
