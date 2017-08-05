import scala.io.Source

val lines = Source.fromFile("/Users/Madhu/IdeaProjects/opt/src/main/resources/data/tsp/tsp_5_1").getLines().toVector

val numCities = lines.take(1)(0).toInt

val citiesCoordinates = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toDouble, x(1).toDouble)).zipWithIndex.map(x => (x._2,x._1)).toMap

def getDistance(x: (Double,Double), y: (Double,Double)): Double = {
 math.sqrt(math.pow((y._1 - x._1),2) + math.pow((y._2 - x._2),2))
}

getDistance((0,0),(1,1))

citiesCoordinates.get(0).get

val distanceMatrix = Array.tabulate[Double](numCities,numCities)((x,y) => getDistance(citiesCoordinates.get(x).get,citiesCoordinates.get(y).get))

distanceMatrix(0)(1)

distanceMatrix.flatten.sum