import scala.io.Source

val lines = Source.fromFile("/Users/Madhu/IdeaProjects/opt/src/main/resources/data/graphcoloring/gc_100_1").getLines().toVector
val firstLine = lines.take(1)(0).split(" ")

val numNodes = firstLine(0).toInt
val numEdges = firstLine(1).toInt

val edges = lines.drop(1).map(x => x.split(" ")).map(x => (x(0).toInt, x(1).toInt))

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
def getAdjMatrix(edgeList: Vector[(Int, Int)]):Array[Array[Int]] = Array.tabulate(numNodes, numNodes)((x, y) => hasEdge(x, y, edgeList))

val adjMatrix = getAdjMatrix(edges)


val edgesSortedbyDegree:Array[(Int, Int)] = adjMatrix.zipWithIndex map (x => (x._2,x._1.sum)) sortWith(_._2 > _._2)

val maxDegreeNode = edgesSortedbyDegree(0)._1

val colors = edgesSortedbyDegree(0)._2 + 1
