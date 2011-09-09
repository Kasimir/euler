package de.euler.task1

class Task1{
  //Add all the natural numbers below one thousand that are multiples of 3 or 5.
  def solve : Int = {
    val three = 2;
    val five = 4;
    return solveRec(three,five,0)  
  }
  
  def solveRec(three:Int, five:Int, count:Int):Int = {
    if (count == 1000) return 0
    val isThree = three == 2
    val isFive = five == 4
    val result = ((if (isThree || isFive) count else 0) + solveRec((if(isThree) 0 else three+1), (if (isFive) 0 else five+1), count+1))
    return result
  }
}
