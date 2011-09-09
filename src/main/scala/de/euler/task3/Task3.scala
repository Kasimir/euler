package de.euler.task3

class Task3{
//The prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143 ?

  def solve : List[Long] = {
    return factor(target)
  }
   val target:Long = 600851475143L
   val primeList:Stream[Long] = nextPrime(2L)
   
   def nextPrime(start:Long):Stream[Long] = {
       isPrime(start) match {
           case false => nextPrime(start+1L)
           case true => Stream.cons(start, nextPrime(start+1L))
       }
   }
   
   def isPrime(num:Long, mod:Long = 2L):Boolean = {
       num < mod*mod || (num%mod match {
           case 0 => if(num == mod) true else false
           case _ => isPrime(num, mod + 1L)
       })
   }
   
   def factor(num:Long, factors:List[Long] = List()):List[Long] = {
       println("Get factor for "+num)
       primeList takeWhile(_ <= num) find(num%_ == 0) match {
           case None => factors
           case Some(x) => factor(num/x, x::factors)
       }
   }
}