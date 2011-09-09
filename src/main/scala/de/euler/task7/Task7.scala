package de.euler.task7

import de.euler.task3._


class Task7{
//By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

//What is the 10 001st prime number?

  def solve : Long = {
    val generator = new Task3().nextPrime(2)
    return generator.take(10001).last    
  }
}