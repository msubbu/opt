package knapsack

import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.SortedMap
import scala.util.Try

/**
  * Created by msubbu on 5/27/17.
  */
object ZeroOneKnapsack extends LazyLogging {

  /**
    * Produces an estimate based on a linear relaxation of the problem
    * @param values
    * @param weights
    * @param room
    * @return estimate
    */
  def estimate(select: Vector[Double], values: Vector[Double], weights: Vector[Double], room: Double): Double = {

    //baseline estimate: sum up values of items selected so far
    val baselineEstimate = getObj(values, select)

    //remaining weights and values
    val remWeights = weights.drop(select.length)
    val remValues = values.drop(select.length)

    //calculate density vector for remaining items, sort values and weights by descending density
    val remDensity = remValues zip remWeights map (x => Try(if ( x._2 != 0) x._1/x._2 else 0).getOrElse(0.0))
    val remWeightsDensity = remWeights zip remDensity sortWith(_._2 > _._2)

    def estimateAcc(dPrime: Vector[Double], wPrime: Vector[Double], remainingRoom: Double, est: Double): Double = (dPrime, wPrime, remainingRoom) match {

      //ran out of room => return
      case (_,_,0) => est

      //ran out of items, doesn't matter if you have more room or not => return
      case (Vector(), Vector(), _) => est

      //have room, can take the next item => move on to next item
      case (nextDensity +: restDensity, nextWeight +: restWeight, r) if r >= nextWeight => estimateAcc(restDensity, restWeight, room - nextWeight, est + nextDensity * nextWeight)

      //have room, cannot take the next item => take a fraction of next item
      case (nextDensity +: restDensity, nextWeight +: restWeight, r) if r < nextWeight => est + r * nextDensity
    }

    //tail recursive method - start with the best estimate given remaining weights and values
    estimateAcc(remWeightsDensity map (x => x._2), remWeightsDensity map (x => x._1), room, baselineEstimate)

  }


  /**
    * Evaluate total value in knapsack given a value vector and a selection vector
    * (which needs to be appended with trailing zeros)
    * @param values
    * @param select
    * @return totalValue
    */
  def getObj(values: Vector[Double], select: Vector[Double]): Double = values zip (select ++ Vector.fill(values.length - select.length)(0.0)) map (x => x._1 * x._2) sum


  /**
    * Recursively explore state space. Skip a subtree if a) breached capacity constraint or b) the best estimate for this subtree is lower than the best known objective function value
    * @param depth
    * @param room
    * @param values
    * @param weights
    * @return a sorted map of objective function values and corresponding state spaces
    */
  def explore(depth: Int, room: Double, values: Vector[Double], weights: Vector[Double]): SortedMap[Double,Vector[Double]] = {

    //estimate based on LP relaxation
    val initialEstimate = estimate(Vector(),values,weights,room)
    val seedMap: SortedMap[Double,Vector[Double]] = SortedMap.empty(Ordering[Double].reverse) + (0.0 -> Vector())

    //the only var in the program - different parts of the subtree need to be aware of this value
    var bestObj: Double = 0.0


    def exploreAcc(mapSoFar: SortedMap[Double,Vector[Double]], select: Vector[Double], bestEstimate: Double, remainingRoom: Double, values: Vector[Double], weights: Vector[Double]): SortedMap[Double,Vector[Double]] = {

      val thisEstimate = estimate(select, values, weights, remainingRoom)
      val obj = getObj(values, select)

      if(obj > bestObj && remainingRoom >= 0) {
        bestObj = obj

        logger.debug("Selection Vector: "+(select map (x => x.toInt)))
        logger.debug("Estimate at this node: " + thisEstimate)
        logger.debug("Objective at this node: " + obj.toLong)
        logger.debug("Best objective so far: " + bestObj.toLong)
        logger.debug("Remaining Room: " + remainingRoom.toLong)
        logger.debug("Tree depth: "+select.length)
        logger.debug("------------------------------------------------")
      }

      //Explore subtree if there are mote items to pack, have more room and if the best estimate is better than currently known best objective function value
      if (select.length < depth && remainingRoom > 0 && thisEstimate > bestObj) {

        //logger.debug("Exploring left & right children")
        //don't select the item
        exploreAcc(mapSoFar, select ++ Vector(1.0), thisEstimate, remainingRoom - weights(select.length), values, weights) ++ exploreAcc(mapSoFar, select ++ Vector(0.0), thisEstimate, remainingRoom, values, weights)
      }

      else if (remainingRoom >=0) mapSoFar + (obj -> (select ++ Vector.fill(values.length - select.length)(0.0)))
      else mapSoFar

    }
    //tail recursive - start with the best initial estimate and the full set of items
    exploreAcc(seedMap, Vector(),initialEstimate, room, values, weights)
  }

  def solve(numItems: Int, capacity: Double, values: Vector[Double], weights: Vector[Double]) = {

    val result = explore(numItems, capacity, values, weights)

    (result.head._1,result.head._2,result,(result.size*100)/Math.pow(2,numItems))

  }

  def main(args: Array[String]): Unit = {

    //sample problem from Google or-tools documentation
    val values: Vector[Double] = Vector(360, 83, 59, 130, 431, 67, 230, 52, 93,
      125, 670, 892, 600, 38, 48, 147, 78, 256,
      63, 17, 120, 164, 432, 35, 92, 110, 22,
      42, 50, 323, 514, 28, 87, 73, 78, 15,
      26, 78, 210, 36, 85, 189, 274, 43, 33,
      10, 19, 389, 276, 312)

    val weights: Vector[Double] = Vector(7, 0, 30, 22, 80, 94, 11, 81, 70,
      64, 59, 18, 0, 36, 3, 8, 15, 42,
      9, 0, 42, 47, 52, 32, 26, 48, 55,
      6, 29, 84, 2, 4, 18, 56, 7, 29,
      93, 44, 71, 3, 86, 66, 31, 65, 0,
      79, 20, 65, 52, 13)

    val capacity: Int = 850

    //val expectedSoln: Vector[Double] = Vector(1,1,0,1,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,1,0,1,1,0,1,0,0,1,1,1,1,1,1,0,1,0,0,0,1,1,0,1,1,0,1,0,0,1,1,1,0)

/*
   val values: Vector[Double] = Vector(8,10,15,4)
    val weights: Vector[Double] = Vector(4,5,8,3)
    val capacity: Double = 11
    val expectedSoln: Vector[Double] = Vector(0,0,1,1)
*/

    val timeStart = System.currentTimeMillis()
    val (obj:Double, soln:Vector[Double],nodeValues:SortedMap[Double,Vector[Double]],portionOfTree:Double) = ZeroOneKnapsack.solve(values.length, capacity, values, weights)
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
    println("Solution Vector: "+soln map (x=> x.toInt))


    val weightsConsumed = weights zip soln map (x => x._1 * x._2)
    println("Weight of knapsack: " + weightsConsumed.sum.toLong)
    println("total # nodes: "+ Math.pow(2,values.length).toLong)
    println("number of nodes explored: "+nodeValues.size)
    println("Portion of tree explored: "+Math.round(portionOfTree)+"%")
    println("Elapsed Time: "+elapsedTime+" ms")
    //nodeValues map {x => println(x)}
    println("\n************************************************")

  }

}
