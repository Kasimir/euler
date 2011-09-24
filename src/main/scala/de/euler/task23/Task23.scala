package de.euler.task23

import de.euler.task21._
import scala.math._
import scala.collection.immutable.BitSet
class Task23{
  def solve(toInt:Int = 28123):Any = {
    val task21 = new Task21;
    val sums = task21.divisorSumsList(toInt);
    //val abundants = (for(i <- sums;if(i._1 < i._2)) yield i._1).toList.sortWith((x,y) => x < y )
    val abundants = BitSet.empty ++ (for(i <- sums;if(i._1 < i._2)) yield i._1)
    var abundantSums = BitSet.empty ++ (for(i <- abundants; j <-abundants; if(i+j <= toInt)) yield i+j)     
    println("abundant sums " + abundantSums.size)
    val noSum :List[BigInt]= (for(i <- 1 to toInt;
                            if( !abundantSums.contains(i) )
                          )yield BigInt(i)).toList;
    return (noSum.sum,noSum.size,noSum)
  }
}