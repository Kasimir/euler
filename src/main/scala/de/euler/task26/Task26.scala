package de.euler.task26

class Task26{

  class DnR(val digits:List[Int],remainder:Int){}

  def findPeriod(prev:List[DnR]): List[Int] = {
    def makeDivable(z:Int,d:Int):(Int,Int) = {
      if(z<d){
	val result = makeDivable(z*10,d)
	return (result._1, result._2+1)
       }
       return (z,0)
    }
    if(prev.head.remainder == 0) return List()
    if(prev.tail.exists(_.remainder == prev.head.remainder )){
      val index = prev.tail.indexOf(prev.head.remainder)
      return  prev.tail.slice(0,index).flatten
    } else {
      val divs = makeDivable(z,d)
      return findPeriod(new DnR(List.iterate(0,divs._2)(_)::: List(divs._1/d),divs._1%d) ::prev)
    }
  }

  def period(n:Int):List[Int] = {
    
  }
}
