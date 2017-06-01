import java.io.IOException

import com.typesafe.scalalogging.LazyLogging
import knapsack._

import scala.collection.immutable.SortedMap
import scala.io.Source
/**
  * Created by msubbu on 1/26/17.
  */
object knapsacksolver extends LazyLogging {

  def main(args: Array[String]): Unit = {

try {
  println("Reading file:")
  println(args(0))

  val lines = Source.fromFile(args(0)).getLines().toVector
  val firstLine = lines.take(1)(0).split(" ")

  val numItems = firstLine(0).toInt
  val capacity = firstLine(1).toDouble

  val data = lines.drop(1).map(x => x.split(" ")).map(x => (x(0), x(1)))

  val values = data.map(x => x._1.toDouble)

  val weights = data.map(x => x._2.toDouble)

  val (obj:Double, soln:Vector[Double],nodeValues:SortedMap[Double,Vector[Double]],portionOfTree:Double) = ZeroOneKnapsack.solve(numItems, capacity, values, weights)

  println(obj+" 1")
  println("Solution Vector: "+soln map (x=> x.toInt))


}
catch {
  case e:IOException => {
    println(e)
    println("Problem processing the file. Expected format: ")
    println("N K")
    println("v_0 w_0")
    println("v_1 w_1")
    println("...")
    println("v_N-1 w_N-1")

  }
}

  }

}
