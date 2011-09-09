package de.euler.task10

import scala.math._


class Task10{   
  def isPrime(num:BigInt) : Boolean = {
    for(t <- 2 to sqrt(num.toDouble).toInt; if(num % t == 0)) return false
    return true
  }
  
  def nextPrime(num:BigInt) : BigInt= {
    Stream.iterate(num+1)(_+1).find(isPrime(_)) match {
      case Some(x) => {
        return x
      } 
      case None => return 0
    }
  }
  
  def solve():BigInt = {
    val result= Stream.iterate(BigInt(2))(x=>nextPrime(x.toInt)).takeWhile(_ < 2000000)    
    return result.sum
  }
}