package de.euler.task21

import de.euler.task12._
import scala.math._


class Task21{
   
  def divisorSumsList(to:Int):Map[Int,Int] = {
    val factorizer = new Task12
    return Map.empty ++ (for(i <- 1 to to; val primes = factorizer.factor(i); val divisorSum = primes.map(x=> (pow(x._1,x._2 + 1) - 1)/(x._1 - 1) ).product.toInt -i) yield (i ->divisorSum)).toList
  }
    
  def solve():Any = {
    val factorizer = new Task12
    
    val divisorSums = divisorSumsList(10000)
    
    println(divisorSums(220-1))
    println(divisorSums(284-1))
    
    val amicableNumbers = (for(i <- 1 to 10000;
                                val divisorSum = divisorSums(i);                                  
                                if( divisorSum <= 10000 
                                  && divisorSum != i
                                  && divisorSum > 0
                                  && divisorSums(divisorSum) == i)) yield i).toList     
    return (amicableNumbers,amicableNumbers.sum)  
  }
}