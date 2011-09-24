package de.euler.task25

import scala.math._

class Task25{
  def fib(n:Int):BigInt = {
    val fi = BigDecimal(1/sqrt(5)).setScale(1000)
    val sn = BigDecimal((1+sqrt(5))/2).setScale(1000)
    return (fi * (sn.pow(n)) + BigDecimal(0.5)).abs.toBigInt
  }
  
  def fibDigits(n:Int):Int = {
    fib(n).toString.length
  }

  def fibs() : Stream[(BigInt,BigInt,BigInt)] = {
    return Stream.iterate((BigInt(1),BigInt(1),BigInt(1)))(x => (x._1+1, x._3, x._2 + x._3))
  }

  def solveBrute():Any = {
    val f = for(i <- fibs.takeWhile(_._2.toString.length < 1000)) yield i
    return f.last
  }
}
