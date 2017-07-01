package knapsack
import java.io.{BufferedWriter, File, FileWriter, IOException}

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
  print("Reading file: ")
  println(args(0))

  val lines = Source.fromFile(args(0)).getLines().toVector
  val firstLine = lines.take(1)(0).split(" ")

  val numItems = firstLine(0).toInt
  val capacity = firstLine(1).toDouble

  val data = lines.drop(1).map(x => x.split(" ")).map(x => (x(0), x(1)))

  val values = data.map(x => x._1.toDouble)

  val weights = data.map(x => x._2.toDouble)


  val timeStart = System.currentTimeMillis()
  val (obj:Double, soln:Vector[Double],nodeValues:SortedMap[Double,Vector[Double]],portionOfTree:Double) = ZeroOneKnapsack.solve(numItems, capacity, values, weights)
  val elapsedTime = System.currentTimeMillis() - timeStart


  println("\n************************************************")
  println("****************** Problem *********************")
  println("************************************************\n")
  println("# Items: "+values.length)
  println("Capacity: "+capacity.toLong)
  println("Values: " +values map (x=> x.toInt))
  println("Weights: "+(weights map (x=> x.toInt))+"\n")

  println("************************************************")
  println("****************** Solution ********************")
  println("************************************************\n")
  println("Objective: "+obj.toLong)
  val validatedObj = values zip soln map (x => x._1 * x._2)
  println("Validated objective: " + validatedObj.sum.toInt)
  println("Solution Vector: ")
  soln map (x=> print(x.toInt+" "))


  val weightsConsumed = weights zip soln map (x => x._1 * x._2)
  println("\nWeight of knapsack: " + weightsConsumed.sum.toLong)
  println("total # nodes: "+ Math.pow(2,values.length).toLong)
  println("number of nodes explored: "+nodeValues.size)
  println("Portion of tree explored: "+Math.round(portionOfTree)+"%")
  println("Elapsed Time: "+elapsedTime+" ms")
  //nodeValues map {x => println(x)}
  println("\n************************************************")


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
