package de.euler.task14

class Task14{
  def solve(): Any = {
    def collatzLength(num:Long) : Long = {
      if (num == 1) return 1
      num % 2 match {
        case 0 => return 1 + collatzLength(num / 2)
        case _ => return 1 + collatzLength(3*num + 1)
      }
    }
    var longest = (0L,0L)
    for (i <- 1 to 1000000){
      val cl = collatzLength(i)
      if(longest._2 < cl)
        longest = (i,cl)      
    }
    return longest
  }
}