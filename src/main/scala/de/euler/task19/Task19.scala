package de.euler.task19

abstract class Month{def days(year:Int):Int; val nextMonth:Month}
abstract class ThirtyMonth extends Month {override def days(year :Int): Int = 30}
abstract class ThirtyOneMonth extends Month {override def days(year :Int): Int = 31}
object January extends ThirtyOneMonth{override val nextMonth = Febuary}
object Febuary extends Month{
  override def days(year:Int):Int = {
    if(year%400 == 0)
      return 29
    if(year%100 == 0)
       return 28
    if(year%4 == 0)
      return 29
    return 28      
  } 
  override val nextMonth:Month = March
}
object March extends ThirtyOneMonth{override val nextMonth = April}
object April extends ThirtyMonth{override val nextMonth = May}
object May extends ThirtyOneMonth{override val nextMonth = June}
object June extends ThirtyMonth{override val nextMonth = July}
object July extends ThirtyOneMonth{override val nextMonth = August}
object August extends ThirtyOneMonth{override val nextMonth = September}
object September extends ThirtyMonth{override val nextMonth = October}
object October extends ThirtyOneMonth{override val nextMonth = November}
object November extends ThirtyMonth{override val nextMonth = December}
object December extends ThirtyOneMonth{override val nextMonth = January}


class Task19{
  //defines a stream rpresenting the date (y,m,d,totalDays)
  def dayStream:Stream[(Int,Month,Int,Int)] = Stream.iterate((1900,January:Month,1,1))
    {
      x => if(x._3 == x._2.days(x._1)){
              if(x._2 == December){
                (x._1+1, x._2.nextMonth, 1, x._4+1)
              }
              else{
                (x._1, x._2.nextMonth, 1, x._4+1)
              }
          }
          else{
            (x._1,x._2,x._3+1,x._4+1)
          }
    }
  
  def solve:Any = {
    val sundays = for(d <- dayStream.takeWhile(_._1 < 2001);if(d._1 > 1900); if (d._3 == 1 && d._4 %7 == 0)) yield d
    sundays foreach println
    return sundays.size    
  } 
  
}