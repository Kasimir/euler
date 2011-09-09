package de.euler.task17

class Task17{
  
  object Numbers extends Enumeration{
    val Zero = Value
    val One = Value
    val Two = Value
    val Three = Value
    val Four = Value
    val Five = Value
    val Six = Value
    val Seven = Value
    val Eight = Value
    val Nine = Value
    val Ten = Value
    val Eleven = Value
    val Twelve = Value
    val Thirteen = Value
    val Fourteen = Value
    val Fifteen = Value
    val Sixteen = Value
    val Seventeen = Value
    val Eighteen = Value
    val Nineteen = Value
  }
  
  object Tens extends Enumeration{
    val Twenty = Value(2)
    val Thirty = Value(3)
    val Forty = Value(4)
    val Fifty = Value(5)
    val Sixty = Value(6)
    val Seventy = Value(7)
    val Eighty = Value(8)
    val Ninety = Value(9)
  }
    
  object Number{
    def unapply(num:String): Option[Numbers.Value] = {
      if(num.length <= 2 && num.length >= 1){
        val intVal = num.toInt
        if (intVal > 0 && intVal < 20)
          return Some(Numbers(intVal))
      }
      return None
    }
  }
  
  object Ten{
    def unapply(num:String): Option[(Tens.Value,String)] = {
      if(num.length == 2){
        val intVal = num.substring(0,1).toInt
        if(intVal > 1)
          return Some((Tens(intVal),num.substring(1)))
      }
      return None
    }
  }
  
  object Hundred{
    def unapply(num:String):Option[(Numbers.Value,String)] = {
      if(num.length == 3){
        val intVal = num.substring(0,1).toInt
        if(intVal > 0)
          return Some((Numbers(intVal),num.substring(1)))
      }
      return None
    }
  }

  object Thousand{
    def unapply(num:String):Option[(Numbers.Value,String)] = {
      if(num.length == 4){
        val intVal = num.substring(0,1).toInt
        if(intVal > 0)
          return Some((Numbers(intVal),num.substring(1)))
      }
      return None
    }
  }

  def letters(num:String) : (String,Int) = {
    num.toString match {
      case Number(x) => return (x.toString,x.toString.length)
      case Ten(x,s) => {
        val rest = letters(s)
        val numberString = x.toString + " " + rest._1
        return return (numberString,x.toString.length + rest._2)
      }
      case Hundred(x,s) => {
        val rest = letters(s)
        val restAnd = rest match {
          case (_,0) => ("",0)
          case _ => ("and ",3)
        }
        return (x.toString + " hundred " + restAnd._1 + rest._1, x.toString.length + "hundred".length +rest._2 + restAnd._2)
      }
      case Thousand(x,s) => {
        val rest = letters(s)
        return (x.toString + " thousand " + rest._1, x.toString.length + "thousand".length + rest._2)
      }
      case _ => return ("",0)
    }
  }
  
  
  def solve():Any = {
    val numbers = for (i <- 1 to 1000; number = letters(i.toString)) yield number
    numbers foreach println
    return numbers.map(_._2).sum
  }
}