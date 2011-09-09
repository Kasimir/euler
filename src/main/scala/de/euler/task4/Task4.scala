package de.euler.task4

class Task4{
//A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
//Find the largest palindrome made from the product of two 3-digit numbers.

  def solve : Int = {
    return findPalindrome()
  }
  
  def findPalindrome():Int = {
    val result = (100 to 999).flatMap( i => (i to 999).map(i*) ).filter(n => isPalindrome(n)).max               
    return result
  }
  
  def isPalindrome(num:Int):Boolean = {
    def recDigitList(input:Int):List[Int] = {
      if (input < 10) return List(input)
      return input %10 :: recDigitList(input/10)
    }    
    val digitList = recDigitList(num)
    return digitList equals digitList.reverse
  }
}