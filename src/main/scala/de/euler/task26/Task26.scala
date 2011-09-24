package de.euler.task26

class Task26{

  class DnR(val digits:List[Int],val remainder:Int){
    override def toString : String = {
      return "DnR " + digits + " " + remainder
    }
  }

  def findPeriod(prev:List[DnR],d:Int): List[Int] = {
    def makeDivable(z:Int,d:Int):(Int,Int) = {
      if(z<d){
	val result = makeDivable(z*10,d)
	return (result._1, result._2+1)
      }
      return (z,0)
    }
    var remainder = 1
    if(prev != Nil){
      val index = if(prev!= Nil)prev.tail.indexWhere(_.remainder == prev.head.remainder ) else -1      
      if(index > -1){
	println("found " + prev.head.remainder + " " + index +  " " + prev  + " " + prev.tail.slice(0,index+1))
	return  prev.tail.slice(0,index +1).map(_.digits).reverse.flatten
      }
      remainder = prev.head.remainder
    }
    val divs = makeDivable(remainder,d)
    println("divs:" + divs)
    val newDigits = (divs._1/d).toString.toSeq.map(_.asDigit).toList
    println("newDigits" + newDigits)
    val leadingZeros = for(i <- newDigits.length to (divs._2-1)) yield 0
    println("lz" + leadingZeros)
    val zDigits = (leadingZeros.toList ::: newDigits)
    return findPeriod(new DnR(zDigits,divs._1%d) ::prev,d)
  }

  def period(n:Int):List[Int] = {
    return findPeriod(Nil,n)
  }
}
