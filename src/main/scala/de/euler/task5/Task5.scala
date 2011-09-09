package de.euler.task5

class Task5{
//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  
  def solve : Long = {
    var primeSet : List[Long] = Nil
    for(i <- 1 to 20; val primes = factor(i)) primeSet = primeSet union (primes diff primeSet)
    println(primeSet)
    return primeSet.product  
  }
   
   //val target:Long = 600851475143L
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